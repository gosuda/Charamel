module Models = Crush_core.Models
module Config = Crush_core.Config

let model =
  {
    Charm_fantasy.Model.id = "test";
    name = "Test";
    provider = "test";
    context_window = 100_000;
    default_max_tokens = 4096;
    can_reason = true;
    supports_attachments = false;
    cost_in = 3.0;
    cost_out = 15.0;
    cost_cache_read = 0.3;
    cost_cache_write = 3.75;
  }

(* Token-accounting contract, decision 4: [input] is already the normalized,
   cache-exclusive prompt count -- ordinary prompt tokens, excluding cache
   reads and cache writes. Decision 10: [Models.cost] bills each of the four
   buckets (input, output, cache_read, cache_write) at its own per-million
   rate with no subtraction; input must never be reduced by cache_read a
   second time inside the cost formula itself. *)
let test_cost () =
  let usage =
    {
      Charm_fantasy.Usage.input = 1_000_000;
      output = 100_000;
      cache_read = 200_000;
      cache_write = 10_000;
      reasoning = 0;
    }
  in
  let expected =
    ((1_000_000. *. 3.0) +. (100_000. *. 15.0) +. (200_000. *. 0.3) +. (10_000. *. 3.75))
    /. 1_000_000.
  in
  Alcotest.(check (float 1e-9))
    "cost bills every bucket at its own rate without subtracting cache from input"
    expected (Models.cost model usage)

(* Decoder-to-cost integration.

   [Charm_fantasy.Anthropic_codec] and [Charm_fantasy.Responses_codec] are
   not re-exported by charm_fantasy.mli (lib/fantasy/provider.ml:31-35 uses
   them only internally), so there is no public codec entry point to call
   directly. [Charm_fantasy.Provider.stream] is the real public boundary:
   it selects the same codec, drives one HTTP request, and yields the
   decoded [Stream_part.t] sequence. A loopback fixture server -- the same
   idiom test_agent.ml, test_advisor.ml and test_cli.ml already use for
   Anthropic-shaped SSE fixtures -- lets these tests exercise that real
   decoder boundary without a mock of the code under test. *)

let priced_model =
  {
    Charm_fantasy.Model.id = "priced";
    name = "Priced";
    provider = "test";
    context_window = 200_000;
    default_max_tokens = 4096;
    can_reason = false;
    supports_attachments = false;
    cost_in = 2.0;
    cost_out = 3.0;
    cost_cache_read = 0.5;
    cost_cache_write = 2.5;
  }

type fixture = { port : int; body : string }

let start_fixture ~sw ~net body =
  let socket = Eio.Net.listen net ~backlog:8 ~sw (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let port = match Eio.Net.listening_addr socket with `Tcp (_, port) -> port | _ -> 0 in
  let fixture = { port; body } in
  let handle flow _addr =
    let reader = Eio.Buf_read.of_flow ~max_size:65_536 flow in
    let rec read_headers () =
      match Eio.Buf_read.line reader with "" -> () | _ -> read_headers ()
    in
    read_headers ();
    Eio.Flow.copy_string
      (Fmt.str
         "HTTP/1.1 200 OK\r\n\
          content-type: text/event-stream\r\n\
          content-length: %d\r\n\
          \r\n"
         (String.length fixture.body))
      flow;
    Eio.Flow.copy_string fixture.body flow;
    Eio.Flow.close flow
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork ~sw socket ~on_error:raise handle
      done;
      `Stop_daemon);
  fixture

let rec drain acc stream =
  match Eio.Stream.take stream with
  | Charm_fantasy.Stream_part.Finish _ as part -> List.rev (part :: acc)
  | part -> drain (part :: acc) stream

let usage_from ~make_provider body =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fixture = start_fixture ~sw ~net:env#net body in
  let provider = make_provider (Fmt.str "http://127.0.0.1:%d" fixture.port) in
  let stream =
    Charm_fantasy.Provider.stream provider ~sw ~clock:env#clock ~net:env#net
      ~model:priced_model
      [ Charm_fantasy.Message.text Charm_fantasy.Message.User "hi" ]
  in
  match
    List.find_map
      (function Charm_fantasy.Stream_part.Usage u -> Some u | _ -> None)
      (drain [] stream)
  with
  | Some usage -> usage
  | None -> Alcotest.fail "fixture stream produced no usage part"

(* Responses input_tokens (1000) already includes the 800 cached tokens;
   responses_codec's usage_of subtracts them once to report a disjoint input
   of 200 (lib/fantasy/responses_codec.ml:289-301). reasoning (40) is an
   informational subset of output (100, decision 6) and must never be
   billed a second time. *)
let responses_completed_body =
  "event: response.completed\n"
  ^ "data: {\"type\":\"response.completed\",\"response\":{\"usage\":{"
  ^ "\"input_tokens\":1000,\"input_tokens_details\":{\"cached_tokens\":800},"
  ^ "\"output_tokens\":100,\"output_tokens_details\":{\"reasoning_tokens\":40}}}}\n\n"

let test_cost_from_responses_decoder () =
  let usage =
    usage_from responses_completed_body ~make_provider:(fun base_url ->
        Charm_fantasy.Provider.openai_responses ~base_url
          ~auth:(Charm_fantasy.Provider.Api_key "test-key") ())
  in
  Alcotest.(check (float 1e-9))
    "responses decoder already excludes cache reads from input; Models.cost must not \
     subtract them again"
    0.0011
    (Models.cost priced_model usage);
  Alcotest.(check int)
    "the four billing buckets sum to the decoded total" 1100
    (Charm_fantasy.Usage.total usage)

(* Anthropic reports input_tokens, cache_creation_input_tokens and
   cache_read_input_tokens as disjoint counters (never overlapping;
   lib/fantasy/anthropic_codec.ml:292-298), so Models.cost must bill all
   three, plus output, in full rather than netting cache_read out of
   input. *)
let anthropic_disjoint_body =
  "event: message_start\n"
  ^ "data: \
     {\"type\":\"message_start\",\"message\":{\"id\":\"usage-model\",\"type\":\"message\","
  ^ "\"role\":\"assistant\",\"model\":\"usage-model\",\"content\":[],\"stop_reason\":null,"
  ^ "\"usage\":{\"input_tokens\":100,\"cache_creation_input_tokens\":300,"
  ^ "\"cache_read_input_tokens\":600,\"output_tokens\":0}}}\n\n"
  ^ "event: message_delta\n"
  ^ "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},"
  ^ "\"usage\":{\"output_tokens\":100}}\n\n" ^ "event: message_stop\n"
  ^ "data: {\"type\":\"message_stop\"}\n\n"

let test_cost_from_anthropic_decoder () =
  let usage =
    usage_from anthropic_disjoint_body ~make_provider:(fun base_url ->
        Charm_fantasy.Provider.anthropic ~base_url
          ~auth:(Charm_fantasy.Provider.Api_key "test-key") ())
  in
  Alcotest.(check (float 1e-9))
    "anthropic's disjoint input/read/write buckets must each be billed once, not netted \
     against input"
    0.00155
    (Models.cost priced_model usage);
  Alcotest.(check int)
    "the four billing buckets sum to the decoded total" 1100
    (Charm_fantasy.Usage.total usage)

let test_error_printer () =
  let rendered = Fmt.str "%a" Models.pp_error (`Unknown_model ("anthropic", "missing")) in
  Alcotest.(check string)
    "error names provider and model" "unknown model missing for provider anthropic"
    rendered

let test_with_auth_preserves_selection () =
  Eio_main.run @@ fun env ->
  let root_name = Fmt.str "/tmp/crush-models-%d-%d" (Unix.getpid ()) (Random.bits ()) in
  let root = Eio.Path.(env#fs / root_name) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)
    (fun () ->
      let selection : Config.selected_model =
        {
          provider = "test";
          model = "test";
          reasoning = Some `High;
          max_tokens = Some 777;
        }
      in
      let configured : Config.provider =
        {
          kind = Config.Openai_compatible;
          base_url = Some "https://example.invalid/v1";
          api_key = None;
          headers = [];
          models = [ model ];
        }
      in
      let config =
        {
          Config.default with
          providers = [ ("test", configured) ];
          models = { Config.default.Config.models with large = Some selection };
        }
      in
      let path = Eio.Path.(root / "auth.json") in
      match Crush_core.Auth.create ~path ~clock:env#clock () with
      | Error error ->
          Alcotest.failf "auth resource creation failed: %a" Crush_core.Auth.pp_error
            error
      | Ok auth -> (
          match
            Crush_core.Auth.set auth ~provider:"test" (Crush_core.Auth.Api_key "old")
          with
          | Error error ->
              Alcotest.failf "auth setup failed: %a" Crush_core.Auth.pp_error error
          | Ok () -> (
              match
                Models.resolve ~fs:env#fs config ~auth ~env:(fun _ -> None) ~role:`Large
              with
              | Error error ->
                  Alcotest.failf "model resolution failed: %a" Models.pp_error error
              | Ok resolved -> (
                  match
                    Models.with_auth ~fs:env#fs config
                      ~env:(fun _ -> None)
                      resolved (Charm_fantasy.Provider.Api_key "new")
                  with
                  | Error error ->
                      Alcotest.failf "provider rebind failed: %a" Models.pp_error error
                  | Ok rebound ->
                      Alcotest.(check string)
                        "provider identity" resolved.Models.provider_id
                        rebound.Models.provider_id;
                      Alcotest.(check string)
                        "model identity" resolved.Models.model.Charm_fantasy.Model.id
                        rebound.Models.model.Charm_fantasy.Model.id;
                      Alcotest.(check int)
                        "max tokens" resolved.Models.max_tokens rebound.Models.max_tokens;
                      Alcotest.(check bool)
                        "same role" true
                        (resolved.Models.role = rebound.Models.role);
                      Alcotest.(check bool)
                        "same reasoning" true
                        (resolved.Models.reasoning = rebound.Models.reasoning)))))

let cases =
  [
    Alcotest.test_case "usage cost" `Quick test_cost;
    Alcotest.test_case "responses decoder cost and total" `Quick
      test_cost_from_responses_decoder;
    Alcotest.test_case "anthropic decoder cost and total" `Quick
      test_cost_from_anthropic_decoder;
    Alcotest.test_case "selection error" `Quick test_error_printer;
    Alcotest.test_case "rebind preserves selection" `Quick
      test_with_auth_preserves_selection;
  ]
