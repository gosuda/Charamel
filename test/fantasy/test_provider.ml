open Lwt.Infix
open Charamel_fantasy

let model : Model.t =
  {
    id = "claude-sonnet-4-20250514";
    name = "fixture";
    provider = "anthropic";
    context_window = 200_000;
    default_max_tokens = 4096;
    can_reason = false;
    supports_attachments = false;
    cost_in = 0.;
    cost_out = 0.;
    cost_cache_read = 0.;
    cost_cache_write = 0.;
  }

let user_message = Message.text Message.User "hello"

let complete_body =
  {|event: message_start
data: {"type":"message_start","message":{"usage":{"input_tokens":1}}}

event: content_block_start
data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"ok"}}

event: content_block_stop
data: {"type":"content_block_stop","index":0}

event: message_delta
data: {"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":1}}

event: message_stop
data: {"type":"message_stop"}

|}

let split_body =
  {|event: message_start
data: {"type":"message_start","message":
data: {"usage":{"input_tokens":1}}}

event: content_block_start
data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"split"}}

event: content_block_stop
data: {"type":"content_block_stop","index":0}

event: message_delta
data: {"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":1}}

event: message_stop
data: {"type":"message_stop"}

|}

let partial_body =
  {|event: message_start
data: {"type":"message_start","message":{}}

event: content_block_start
data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"partial"}}

|}

let clock = Charamel_os.Time.lwt
let base_url server = Uri.to_string (Fixture_http.uri server "")

let provider server =
  Provider.anthropic ~base_url:(base_url server) ~auth:(Provider.Api_key "fixture-key") ()

let stream_parts server messages =
  Provider.stream (provider server) ~clock ~model messages |> Stream_test_support.drain

let has_text text parts =
  List.exists
    (function Stream_part.Text_delta value -> String.equal value text | _ -> false)
    parts

let has_finish reason parts =
  List.exists (function Stream_part.Finish value -> value = reason | _ -> false) parts

let ended_with_error parts =
  match List.rev parts with Stream_part.Finish (`Error _) :: _ -> true | _ -> false

let carries name value headers =
  List.exists
    (fun (key, actual) -> String.equal key name && String.equal actual value)
    headers

let test_split_sse =
  Alcotest_lwt.test_case "split SSE data" `Quick (fun _switch () ->
      Stream_test_support.with_fixture (fun server ->
          Fixture_http.respond server split_body;
          stream_parts server [ user_message ] >|= fun parts ->
          Alcotest.(check bool)
            "multiline SSE data reaches codec" true (has_text "split" parts);
          Alcotest.(check int) "one request" 1 (Fixture_http.requests server)))

let test_headers =
  Alcotest_lwt.test_case "provider headers" `Quick (fun _switch () ->
      Stream_test_support.with_fixture (fun server ->
          Fixture_http.respond server complete_body;
          stream_parts server [ user_message ] >|= fun _parts ->
          let headers = Fixture_http.last_headers server in
          Alcotest.(check bool)
            "Anthropic version header" true
            (carries "anthropic-version" "2023-06-01" headers);
          Alcotest.(check bool)
            "API key header" true
            (carries "x-api-key" "fixture-key" headers)))

let oauth_credential =
  {
    Oauth.Credential.access = "oauth-access";
    refresh = "";
    expires_at_ms = 4_000_000_000_000;
    account = None;
  }

let oauth_bearer_case name make_provider =
  Alcotest_lwt.test_case name `Quick (fun _switch () ->
      Stream_test_support.with_fixture (fun server ->
          Fixture_http.respond server complete_body;
          let provider = make_provider server in
          Provider.stream provider ~clock ~model [ user_message ]
          |> Stream_test_support.drain
          >|= fun _parts ->
          Alcotest.(check bool)
            "bearer authorization header" true
            (carries "authorization" "Bearer oauth-access"
               (Fixture_http.last_headers server))))

let test_openai_oauth_header =
  oauth_bearer_case "openai oauth bearer header" (fun server ->
      Provider.openai_compatible ~base_url:(base_url server)
        ~auth:(Provider.Oauth oauth_credential) ())

let test_google_oauth_header =
  oauth_bearer_case "google oauth bearer header" (fun server ->
      Provider.google ~base_url:(base_url server) ~auth:(Provider.Oauth oauth_credential)
        ())

let test_retry_after =
  Alcotest_lwt.test_case "Retry-After" `Quick (fun _switch () ->
      Stream_test_support.with_fixture (fun server ->
          Fixture_http.respond server ~status:429 ~retry_after:0.1 "busy";
          Fixture_http.respond server complete_body;
          let started = Charamel_os.Time.now clock in
          stream_parts server [ user_message ] >|= fun parts ->
          let elapsed = Charamel_os.Time.now clock -. started in
          Alcotest.(check bool) "retry-after delay is observed" true (elapsed >= 0.08);
          Alcotest.(check int)
            "retry makes one follow-up request" 2
            (Fixture_http.requests server);
          Alcotest.(check bool) "retry ends successfully" true (has_finish `Stop parts)))

let test_no_retry_after_emission =
  Alcotest_lwt.test_case "no retry after emission" `Quick (fun _switch () ->
      Stream_test_support.with_fixture (fun server ->
          Fixture_http.respond server partial_body;
          stream_parts server [ user_message ] >|= fun parts ->
          Alcotest.(check int)
            "emitted stream is not retried" 1
            (Fixture_http.requests server);
          Alcotest.(check bool)
            "premature EOF is terminal error" true (ended_with_error parts)))

let test_malformed_body =
  Alcotest_lwt.test_case "malformed body" `Quick (fun _switch () ->
      Stream_test_support.with_fixture (fun server ->
          Fixture_http.respond server "event: message_start\ndata: not-json\n\n";
          stream_parts server [ user_message ] >|= fun parts ->
          Alcotest.(check bool)
            "malformed SSE is an error finish" true (ended_with_error parts)))

(* [next_answered] is [None] when the stream reached its end without another event, which
   is what a stopped producer leaves behind. *)
let next_answered stream =
  Lwt.catch
    (fun () -> Lwt_stream.next stream >|= fun _part -> Some ())
    (function Lwt_stream.Empty -> Lwt.return None | exn -> Lwt.fail exn)

(* The provider is told to wait thirty seconds before its retry. The consumer's switch,
   turned off a moment later, must end the stream rather than leave the reader waiting out
   that delay: an interrupted turn that released neither the fiber nor the connection is the
   defect this pins. The five second bound only turns a regression into a failure instead of
   a hang. *)
let stop_during_retry server =
  Lwt_switch.with_switch (fun stop ->
      let stream =
        Provider.stream (provider server) ~stop ~clock ~model [ user_message ]
      in
      Lwt.async (fun () -> Lwt_unix.sleep 0.05 >>= fun () -> Lwt_switch.turn_off stop);
      next_answered stream)

let test_deadline_cancellation =
  Alcotest_lwt.test_case "deadline cancellation" `Quick (fun _switch () ->
      Stream_test_support.with_fixture (fun server ->
          Fixture_http.respond server ~status:429 ~retry_after:30. "busy";
          Lwt.catch
            (fun () ->
              Lwt_unix.with_timeout 5. (fun () -> stop_during_retry server)
              >|= fun answer -> Some answer)
            (function Lwt_unix.Timeout -> Lwt.return None | exn -> Lwt.fail exn)
          >>= fun outcome ->
          Alcotest.(check bool)
            "a stopped stream ends instead of waiting out the retry" true
            (match outcome with Some None -> true | _ -> false);
          Alcotest.(check int)
            "the deadline test starts one request" 1
            (Fixture_http.requests server);
          Lwt.return_unit))

let cases =
  [
    test_split_sse;
    test_headers;
    test_openai_oauth_header;
    test_google_oauth_header;
    test_retry_after;
    test_no_retry_after_emission;
    test_malformed_body;
    test_deadline_cancellation;
  ]
