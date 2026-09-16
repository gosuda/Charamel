module Advisor = Crush_core.Advisor
module Config = Crush_core.Config

let message_start model_id =
  Fmt.str
    "event: message_start\n\
     data: \
     {\"type\":\"message_start\",\"message\":{\"id\":\"advisor\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"%s\",\"content\":[],\"stop_reason\":null,\"usage\":{\"input_tokens\":1,\"output_tokens\":0}}}\n\n"
    model_id

type fixture = { port : int; response : string }

let start_fixture ~sw ~net response =
  let socket = Eio.Net.listen net ~backlog:8 ~sw (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let port = match Eio.Net.listening_addr socket with `Tcp (_, port) -> port | _ -> 0 in
  let fixture = { port; response } in
  let handle flow _addr =
    let reader = Eio.Buf_read.of_flow ~max_size:65_536 flow in
    let rec read_headers () =
      match Eio.Buf_read.line reader with "" -> () | _ -> read_headers ()
    in
    read_headers ();
    let body = fixture.response in
    Eio.Flow.copy_string
      (Fmt.str
         "HTTP/1.1 200 OK\r\n\
          content-type: text/event-stream\r\n\
          content-length: %d\r\n\
          \r\n"
         (String.length body))
      flow;
    Eio.Flow.copy_string body flow;
    Eio.Flow.close flow
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork ~sw socket ~on_error:raise handle
      done;
      `Stop_daemon);
  fixture

let response model_id severity guidance =
  message_start model_id
  ^ "event: content_block_start\n\
     data: \
     {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
  ^ Fmt.str
      "event: content_block_delta\n\
       data: \
       {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":%S}}\n\n"
      (Fmt.str "{\"severity\":%S,\"guidance\":%S}" severity guidance)
  ^ "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  ^ "event: message_delta\n\
     data: \
     {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}\n\n"
  ^ "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"

let model ~base_url =
  let model : Charm_fantasy.Model.t =
    {
      id = "advisor-model";
      name = "Advisor fixture";
      provider = "anthropic";
      context_window = 100_000;
      default_max_tokens = 512;
      can_reason = false;
      supports_attachments = false;
      cost_in = 0.;
      cost_out = 0.;
      cost_cache_read = 0.;
      cost_cache_write = 0.;
    }
  in
  let provider =
    Charm_fantasy.Provider.anthropic ~base_url
      ~auth:(Charm_fantasy.Provider.Api_key "fixture-key") ()
  in
  {
    Crush_core.Models.role = `Small;
    provider_id = "anthropic";
    provider;
    model;
    reasoning = `Off;
    max_tokens = 512;
  }

let test_json_codec () =
  let verdict = { Advisor.severity = `Concern; guidance = "review the boundary" } in
  match Jsont.Json.encode Advisor.verdict_jsont verdict with
  | Error message -> Alcotest.failf "verdict encoding failed: %s" message
  | Ok json -> (
      match Jsont.Json.decode Advisor.verdict_jsont json with
      | Error message -> Alcotest.failf "verdict decoding failed: %s" message
      | Ok decoded ->
          Alcotest.(check string)
            "guidance survives codec" verdict.Advisor.guidance decoded.Advisor.guidance)

let test_quarantine () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let model_id = "advisor-model" in
  let fixture =
    start_fixture ~sw ~net:env#net (response model_id "concern" "same advice")
  in
  let provider = model ~base_url:(Fmt.str "http://127.0.0.1:%d" fixture.port) in
  let advisor =
    Advisor.create { Config.enabled = true; model = `Small; every_n_turns = 1 }
  in
  let turn =
    [ Charm_fantasy.Message.text Charm_fantasy.Message.User "change the file" ]
  in
  let review () =
    Advisor.review advisor ~sw ~clock:env#clock ~net:env#net provider ~context:"rules"
      ~last_turn:turn
  in
  (match review () with
  | Ok (Some verdict) ->
      Alcotest.(check string) "first verdict" "same advice" verdict.Advisor.guidance
  | Ok None -> Alcotest.fail "first verdict was discarded"
  | Error (`Provider message) -> Alcotest.failf "provider failed: %s" message);
  (match review () with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "second identical verdict was not quarantined"
  | Error (`Provider message) -> Alcotest.failf "provider failed: %s" message);
  Advisor.reset advisor;
  match review () with
  | Ok (Some _) -> ()
  | Ok None -> Alcotest.fail "reset did not clear quarantine"
  | Error (`Provider message) -> Alcotest.failf "provider failed: %s" message

let cases =
  [
    Alcotest.test_case "verdict codec" `Quick test_json_codec;
    Alcotest.test_case "duplicate verdict quarantine" `Quick test_quarantine;
  ]
