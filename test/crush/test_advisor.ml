module Advisor = Crush_core.Advisor
module Config = Crush_core.Config

let message_start model_id =
  Fmt.str
    "event: message_start\n\
     data: \
     {\"type\":\"message_start\",\"message\":{\"id\":\"advisor\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"%s\",\"content\":[],\"stop_reason\":null,\"usage\":{\"input_tokens\":1,\"output_tokens\":0}}}\n\n"
    model_id

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
  let model : Charamel_fantasy.Model.t =
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
    Charamel_fantasy.Provider.anthropic ~base_url
      ~auth:(Charamel_fantasy.Provider.Api_key "fixture-key") ()
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
  let sw = Lwt_switch.create () in
  let clock = Charamel_os.Time.lwt in
  let model_id = "advisor-model" in
  Test_tools_test_support.with_http_fixture ~content_type:"text/event-stream"
    (response model_id "concern" "same advice") (fun port ->
      let provider = model ~base_url:(Fmt.str "http://127.0.0.1:%d" port) in
      let advisor =
        Advisor.create { Config.enabled = true; model = `Small; every_n_turns = 1 }
      in
      let turn =
        [ Charamel_fantasy.Message.text Charamel_fantasy.Message.User "change the file" ]
      in
      let review () =
        Lwt_direct.await
        @@ Advisor.review advisor ~sw ~clock provider ~context:"rules" ~last_turn:turn
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
      | Error (`Provider message) -> Alcotest.failf "provider failed: %s" message)

let cases =
  [
    Test_tools_test_support.case "verdict codec" `Quick test_json_codec;
    Test_tools_test_support.case "duplicate verdict quarantine" `Quick test_quarantine;
  ]
