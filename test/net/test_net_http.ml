open Lwt.Infix

let transport_failure message : Charamel_net.error = `Transport message
let oauth_failure message : Charamel_net.error = `Oauth message

let get fixture ?timeout ?headers path =
  Charamel_net.call ?timeout ?headers ~meth:`GET ~body:None
    (Fixture_http.uri fixture path)

let bounds =
  Alcotest_lwt.test_case "publishes the documented bounds" `Quick (fun _switch () ->
      Alcotest.(check int) "SSE line" (64 * 1024) Charamel_net.max_sse_line;
      Alcotest.(check int) "SSE event" (1024 * 1024) Charamel_net.max_sse_event;
      Alcotest.(check int) "SSE event name" 256 Charamel_net.sse_event_name_bound;
      Alcotest.(check int) "body" (10 * 1024 * 1024) Charamel_net.max_http_body;
      Alcotest.(check int) "error body" 4096 Charamel_net.max_error_body;
      Alcotest.(check (float 0.)) "attempt timeout" 30. Charamel_net.attempt_timeout;
      Lwt.return_unit)

let streaming =
  Alcotest_lwt.test_case "streams the body in wire chunks" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond_chunks fixture [ "alpha "; "beta"; " gamma" ];
          get fixture "/v1/stream" >>= fun result ->
          Alcotest.(check int) "status" 200 (Net_test_support.status_code "call" result);
          Net_test_support.collect (Net_test_support.stream_of "call" result)
          >>= fun chunks ->
          Alcotest.(check (list string)) "chunks" [ "alpha "; "beta"; " gamma" ] chunks;
          Lwt.return_unit))

let incremental =
  Alcotest_lwt.test_case "delivers a chunk before the response ends" `Quick
    (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond_chunks fixture ~hold:2. [ "data: one\n\n" ];
          get fixture "/v1/events" >>= fun result ->
          let stream = Net_test_support.stream_of "call" result in
          Lwt_stream.next stream >>= fun first ->
          Alcotest.(check string) "first chunk" "data: one\n\n" first;
          Alcotest.(check bool)
            "no second chunk yet" true
            (List.is_empty (Lwt_stream.get_available stream));
          Lwt.return_unit))

let split_event =
  Alcotest_lwt.test_case "reassembles one event split across chunks" `Quick
    (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond_chunks fixture [ "data: hel"; "lo\n\n" ];
          get fixture "/v1/events" >>= fun result ->
          Net_test_support.read_events (Net_test_support.stream_of "call" result)
          >>= fun (status, events) ->
          Net_test_support.check_ok "read_sse" Alcotest.unit status () >>= fun () ->
          Alcotest.check Net_test_support.events "events" [ ("message", "hello") ] events;
          Lwt.return_unit))

let trailing_event =
  Alcotest_lwt.test_case "dispatches the pending event at end of stream" `Quick
    (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond fixture
            "event: delta\r\ndata: one\r\ndata: two\r\n\r\n:ping\r\n";
          get fixture "/v1/events" >>= fun result ->
          Net_test_support.read_events (Net_test_support.stream_of "call" result)
          >>= fun (status, events) ->
          Net_test_support.check_ok "read_sse" Alcotest.unit status () >>= fun () ->
          Alcotest.check Net_test_support.events "events" [ ("delta", "one\ntwo") ] events;
          Lwt.return_unit))

let request_shape =
  Alcotest_lwt.test_case "sends the request body and headers" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond fixture "{}";
          Charamel_net.call
            ~headers:
              [ ("authorization", "Bearer token"); ("content-type", "application/json") ]
            ~meth:`POST ~body:(Some "{\"q\":1}")
            (Fixture_http.uri fixture "/v1/search")
          >>= fun result ->
          Alcotest.(check int) "status" 200 (Net_test_support.status_code "call" result);
          Alcotest.(check (option string))
            "path" (Some "/v1/search")
            (Fixture_http.last_path fixture);
          Alcotest.(check (option string))
            "body" (Some "{\"q\":1}")
            (Fixture_http.last_body fixture);
          Alcotest.(check (option string))
            "authorization" (Some "Bearer token")
            (List.assoc_opt "authorization" (Fixture_http.last_headers fixture));
          Lwt.return_unit))

let oversized_line =
  Alcotest_lwt.test_case "rejects an SSE line above 64 KiB" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond_chunks fixture
            [ String.make 40_000 'x'; String.make 40_000 'x'; "\n" ];
          get fixture "/v1/events" >>= fun result ->
          Charamel_net.read_sse (Net_test_support.stream_of "call" result)
            ~on_event:(fun _event -> ())
          >>= fun status ->
          Net_test_support.check_transport "line bound" "SSE line exceeds 64 KiB" status))

let oversized_event =
  Alcotest_lwt.test_case "rejects an SSE event above 1 MiB" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          let line = "data: " ^ String.make 32_000 'y' ^ "\n" in
          Fixture_http.respond_chunks fixture (List.init 40 (fun _index -> line));
          get fixture "/v1/events" >>= fun result ->
          Charamel_net.read_sse (Net_test_support.stream_of "call" result)
            ~on_event:(fun _event -> ())
          >>= fun status ->
          Net_test_support.check_transport "event bound" "SSE event data exceeds 1 MiB"
            status))

let oversized_event_name =
  Alcotest_lwt.test_case "rejects an SSE event name above 256 bytes" `Quick
    (fun _switch () ->
      let stream =
        Lwt_stream.of_list [ "event: " ^ String.make 300 'e' ^ "\n"; "data: x\n\n" ]
      in
      Charamel_net.read_sse stream ~on_event:(fun _event -> ()) >>= fun status ->
      Net_test_support.check_transport "name bound" "SSE event name exceeds 256 bytes"
        status)

let oversized_body =
  Alcotest_lwt.test_case "rejects a body above 10 MiB" `Quick (fun _switch () ->
      let piece = String.make 1_048_576 'a' in
      Charamel_net.read_body (Lwt_stream.of_list (List.init 11 (fun _index -> piece)))
      >>= fun body ->
      Net_test_support.check_transport "body bound" "HTTP response body exceeds 10 MiB"
        body)

let accepted_body =
  Alcotest_lwt.test_case "accepts a body at the size bound" `Quick (fun _switch () ->
      Charamel_net.read_body
        (Lwt_stream.of_list [ String.make Charamel_net.max_http_body 'a' ])
      >|= fun body ->
      Alcotest.(check int)
        "at the bound" Charamel_net.max_http_body
        (body |> Net_test_support.value_of "body" |> String.length))

let accepted_line =
  Alcotest_lwt.test_case "accepts an SSE line at the length bound" `Quick
    (fun _switch () ->
      let stream =
        Lwt_stream.of_list [ String.make Charamel_net.max_sse_line 'x'; "\n" ]
      in
      Charamel_net.read_sse stream ~on_event:(fun _event -> ()) >>= fun status ->
      Net_test_support.check_ok "line at the bound" Alcotest.unit status ())

let oversized_error_body =
  Alcotest_lwt.test_case "omits an error body above 4 KiB" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond ~status:500 fixture (String.make 5_000 'e');
          get fixture "/v1/fail" >>= fun result ->
          Net_test_support.check_http "error body" 500 "Internal Server Error"
            "<response body omitted: too large>" true None result))

let oversized_raw_line =
  Alcotest_lwt.test_case "rejects a raw channel line above the bound" `Quick
    (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond fixture (String.make 70_000 'z');
          Charamel_net.call_raw ~meth:`GET ~body:None (Fixture_http.uri fixture "/v1/raw")
          >>= fun result ->
          let channel =
            match result with
            | Ok (_response, channel) -> channel
            | Error _ -> Alcotest.fail "expected a raw response channel"
          in
          Charamel_net.read_line channel >>= fun line ->
          Net_test_support.check_transport "line bound"
            "response line exceeds 65536 bytes" line))

let retry_after_forms =
  Alcotest_lwt.test_case "parses Retry-After and retry-after-ms" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond ~status:429 ~retry_after:3. fixture "slow down";
          get fixture "/v1/chat" >>= fun seconds ->
          Net_test_support.check_http "seconds" 429 "Too Many Requests" "slow down" true
            (Some 3.) seconds
          >>= fun () ->
          Fixture_http.respond ~status:503
            ~headers:[ ("retry-after-ms", "250"); ("retry-after", "9") ]
            fixture "";
          get fixture "/v1/chat" >>= fun milliseconds ->
          Net_test_support.check_http "milliseconds" 503 "Service Unavailable"
            "empty response body" true (Some 0.25) milliseconds
          >>= fun () ->
          Fixture_http.respond ~status:500
            ~headers:[ ("retry-after", "Mon, 07 Nov 1994 08:49:37 GMT") ]
            fixture "";
          get fixture "/v1/chat" >>= fun dated ->
          Net_test_support.check_http "date" 500 "Internal Server Error"
            "empty response body" true (Some 0.) dated
          >>= fun () ->
          Fixture_http.respond ~status:500 ~headers:[ ("retry-after", "soon") ] fixture "";
          get fixture "/v1/chat" >>= fun malformed ->
          Net_test_support.check_http "malformed" 500 "Internal Server Error"
            "empty response body" true None malformed
          >>= fun () ->
          Fixture_http.respond ~retry_after:2. fixture "ok";
          get fixture "/v1/chat" >>= fun success ->
          let response = Net_test_support.value_of "success" (Result.map fst success) in
          Alcotest.(check (option (float 0.)))
            "success header" (Some 2.)
            (Charamel_net.retry_after response);
          Lwt.return_unit))

let retry_honours_retry_after =
  Alcotest_lwt.test_case "retries a rate-limited call with its delay" `Quick
    (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond ~status:429 ~retry_after:0.5 fixture "";
          Fixture_http.respond fixture "second attempt";
          let delays = ref [] in
          let sleep seconds =
            delays := seconds :: !delays;
            Lwt.return_unit
          in
          Charamel_net.retry ~sleep (fun () -> get fixture "/v1/chat") >>= fun result ->
          Charamel_net.read_body (Net_test_support.stream_of "retry" result)
          >>= fun body ->
          Net_test_support.check_ok "body" Alcotest.string body "second attempt"
          >>= fun () ->
          Alcotest.(check (list (float 0.))) "delay" [ 0.5 ] (List.rev !delays);
          Alcotest.(check int) "attempts" 2 (Fixture_http.requests fixture);
          Lwt.return_unit))

let retry_skips_permanent =
  Alcotest_lwt.test_case "does not retry a permanent status" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond ~status:400 fixture "bad request";
          Charamel_net.retry (fun () -> get fixture "/v1/chat") >>= fun result ->
          Net_test_support.check_http "400" 400 "Bad Request" "bad request" false None
            result
          >>= fun () ->
          Alcotest.(check int) "attempts" 1 (Fixture_http.requests fixture);
          Lwt.return_unit))

let retry_transport_ceiling =
  Alcotest_lwt.test_case "retries transport failures to the policy ceiling" `Quick
    (fun _switch () ->
      let policy =
        Charamel_net.Retry.policy ~max:2 ~base:0.001 ~factor:2. ~max_delay:0.01 ~jitter:0.
      in
      let calls = ref 0 in
      Charamel_net.retry ~policy
        ~sleep:(fun _seconds -> Lwt.return_unit)
        (fun () ->
          calls := !calls + 1;
          Lwt.return (Error (transport_failure "connection refused")))
      >>= fun result ->
      Net_test_support.check_transport "gave up" "connection refused" result >>= fun () ->
      Alcotest.(check int) "attempts" 3 !calls;
      Lwt.return_unit)

let retry_skips_oauth =
  Alcotest_lwt.test_case "does not retry an OAuth failure" `Quick (fun _switch () ->
      let calls = ref 0 in
      Charamel_net.retry (fun () ->
          calls := !calls + 1;
          Lwt.return (Error (oauth_failure "expired")))
      >>= fun result ->
      (match result with
      | Error (`Oauth message) -> Alcotest.(check string) "oauth" "expired" message
      | Error _ -> Alcotest.fail "expected an OAuth failure"
      | Ok _ -> Alcotest.fail "expected a failure");
      Alcotest.(check int) "attempts" 1 !calls;
      Lwt.return_unit)

let policy_delays =
  Alcotest_lwt.test_case "computes policy delays" `Quick (fun _switch () ->
      let zero () = 0. in
      let one () = 1. in
      let policy = Charamel_net.Retry.default in
      let named ~rng attempt retry_after =
        Charamel_net.Retry.delay ~rng policy ~attempt ~retry_after
      in
      Alcotest.(check (float 0.)) "retry-after wins" 5. (named ~rng:zero 3 (Some 5.));
      Alcotest.(check (float 0.)) "ceiling falls back" 1.5 (named ~rng:zero 3 (Some 500.));
      Alcotest.(check (float 0.))
        "backoff low edge" (0.5 *. 0.75) (named ~rng:zero 1 None);
      Alcotest.(check (float 0.))
        "backoff high edge" (0.5 *. 1.25) (named ~rng:one 1 None);
      Alcotest.(check bool) "408 retryable" true (Charamel_net.Retry.retryable_status 408);
      Alcotest.(check bool) "429 retryable" true (Charamel_net.Retry.retryable_status 429);
      Alcotest.(check bool) "500 retryable" true (Charamel_net.Retry.retryable_status 500);
      Alcotest.(check bool)
        "404 permanent" false
        (Charamel_net.Retry.retryable_status 404);
      Alcotest.(check bool)
        "200 permanent" false
        (Charamel_net.Retry.retryable_status 200);
      Lwt.return_unit)

let x_should_retry =
  Alcotest_lwt.test_case "honours the x-should-retry header" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond ~status:400
            ~headers:[ ("x-should-retry", "true") ]
            fixture "nope";
          get fixture "/v1/chat" >>= fun result ->
          Net_test_support.check_http "x-should-retry" 400 "Bad Request" "nope" true None
            result))

let attempt_timeout =
  Alcotest_lwt.test_case "times out an attempt before the response head" `Quick
    (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond ~before:2. fixture "late";
          get fixture ~timeout:0.05 "/v1/slow" >>= fun result ->
          Net_test_support.check_transport "attempt" "request timed out" result))

let body_timeout =
  Alcotest_lwt.test_case "times out a stalled body read" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond_chunks fixture ~hold:2. [ "data: one\n\n" ];
          get fixture "/v1/events" >>= fun result ->
          Charamel_net.read_body ~timeout:0.05 (Net_test_support.stream_of "call" result)
          >>= fun body ->
          Net_test_support.check_transport "body" "timed out reading HTTP response body"
            body))

let sse_idle_timeout =
  Alcotest_lwt.test_case "times out an idle SSE line" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond_chunks fixture ~hold:2. [ "data: one\n\n" ];
          get fixture "/v1/events" >>= fun result ->
          let received = ref [] in
          Charamel_net.read_sse ~timeout:0.05 (Net_test_support.stream_of "call" result)
            ~on_event:(fun event -> received := (event.event, event.data) :: !received)
          >>= fun status ->
          Net_test_support.check_transport "idle" "timed out waiting for an SSE event"
            status
          >>= fun () ->
          Alcotest.check Net_test_support.events "events"
            [ ("message", "one") ]
            (List.rev !received);
          Lwt.return_unit))

let raw_lines =
  Alcotest_lwt.test_case "reads a raw channel line by line" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond fixture "event: a\r\ndata: one\r\n\r\n";
          Charamel_net.call_raw ~meth:`GET ~body:None (Fixture_http.uri fixture "/v1/raw")
          >>= fun result ->
          let channel =
            match result with
            | Ok (_response, channel) -> channel
            | Error _ -> Alcotest.fail "expected a raw response channel"
          in
          Charamel_net.read_line channel >>= fun first ->
          Charamel_net.read_line channel >>= fun second ->
          Charamel_net.read_line channel >>= fun third ->
          Charamel_net.read_line channel >>= fun fourth ->
          Lwt_io.close channel >>= fun () ->
          Alcotest.(check (option string))
            "first line" (Some "event: a")
            (Net_test_support.value_of "first" first);
          Alcotest.(check (option string))
            "second line" (Some "data: one")
            (Net_test_support.value_of "second" second);
          Alcotest.(check (option string))
            "blank line" (Some "")
            (Net_test_support.value_of "third" third);
          Alcotest.(check (option string))
            "end of stream" None
            (Net_test_support.value_of "fourth" fourth);
          Lwt.return_unit))

let refused =
  Alcotest_lwt.test_case "reports a refused connection" `Quick (fun _switch () ->
      let target = ref (Uri.of_string "http://127.0.0.1:1/") in
      Fixture_http.with_server (fun fixture ->
          target := Fixture_http.uri fixture "/v1/gone";
          Lwt.return_unit)
      >>= fun () ->
      Charamel_net.call ~meth:`GET ~body:None !target >>= fun result ->
      Net_test_support.check_transport "refused" "connection refused" result)

let bad_uri =
  Alcotest_lwt.test_case "reports a malformed or unsupported URI" `Quick
    (fun _switch () ->
      Charamel_net.call ~meth:`GET ~body:None (Uri.of_string "ftp://example.invalid/file")
      >>= fun result ->
      Net_test_support.check_transport "scheme" "unsupported URI scheme: ftp" result
      >>= fun () ->
      Charamel_net.call ~meth:`GET ~body:None (Uri.of_string "/relative/path")
      >>= fun result ->
      Net_test_support.check_transport "no scheme" "URI has no scheme" result
      >>= fun () ->
      Charamel_net.call ~meth:`GET ~body:None (Uri.of_string "http:///path")
      >>= fun result ->
      Net_test_support.check_transport "no host" "URI has no host" result)

let bad_timeout =
  Alcotest_lwt.test_case "rejects a timeout that is not positive and finite" `Quick
    (fun _switch () ->
      let rejected =
        Invalid_argument "Charamel_net: timeout must be positive and finite"
      in
      let refuse timeout =
        Alcotest.check_raises "timeout" rejected (fun () ->
            ignore
              (Charamel_net.call ~timeout ~meth:`GET ~body:None
                 (Uri.of_string "http://127.0.0.1:1/")))
      in
      refuse 0.;
      refuse (-1.);
      refuse Float.infinity;
      Lwt.return_unit)

let premature_end =
  Alcotest_lwt.test_case "reports a response cut short" `Quick (fun _switch () ->
      Fixture_http.with_server (fun fixture ->
          Fixture_http.respond ~truncate:4 fixture "0123456789abcdef";
          get fixture "/v1/cut" >>= fun result ->
          Charamel_net.read_body (Net_test_support.stream_of "call" result)
          >>= fun body ->
          (match body with
          | Error (`Transport message) ->
              Alcotest.(check bool)
                "premature" true
                (List.mem message
                   [
                     "unexpected end of HTTP response";
                     "connection closed before the response completed";
                   ])
          | Error _ -> Alcotest.fail "expected a Transport failure"
          | Ok _ -> Alcotest.fail "expected the truncated body to fail");
          Lwt.return_unit))

let taxonomy_prints =
  Alcotest_lwt.test_case "prints and unwraps the error taxonomy" `Quick (fun _switch () ->
      let http : Charamel_net.error =
        `Http
          {
            status = 429;
            title = "Too Many Requests";
            message = "slow down";
            retryable = true;
            retry_after = Some 3.;
          }
      in
      Alcotest.(check string)
        "http" "429 Too Many Requests: slow down"
        (Fmt.to_to_string Charamel_net.pp_error http);
      Alcotest.(check string) "http message" "slow down" (Charamel_net.error_message http);
      Alcotest.(check bool) "http retryable" true (Charamel_net.retryable_error http);
      Alcotest.(check string)
        "transport" "transport: refused"
        (Fmt.to_to_string Charamel_net.pp_error (`Transport "refused"));
      Alcotest.(check string)
        "transport message" "refused"
        (Charamel_net.error_message (`Transport "refused"));
      Alcotest.(check bool)
        "transport retryable" true
        (Charamel_net.retryable_error (`Transport "refused"));
      Alcotest.(check string)
        "oauth" "oauth: expired"
        (Fmt.to_to_string Charamel_net.pp_error (`Oauth "expired"));
      Alcotest.(check string)
        "invalid grant" "oauth invalid grant: no"
        (Fmt.to_to_string Charamel_net.pp_error (`Oauth_invalid_grant "no"));
      Alcotest.(check bool)
        "oauth not retryable" false
        (Charamel_net.retryable_error (`Oauth "expired"));
      Lwt.return_unit)

let cases =
  [
    bounds;
    streaming;
    incremental;
    split_event;
    trailing_event;
    request_shape;
    oversized_line;
    oversized_event;
    oversized_event_name;
    oversized_body;
    accepted_body;
    accepted_line;
    oversized_error_body;
    oversized_raw_line;
    retry_after_forms;
    retry_honours_retry_after;
    retry_skips_permanent;
    retry_transport_ceiling;
    retry_skips_oauth;
    policy_delays;
    x_should_retry;
    attempt_timeout;
    body_timeout;
    sse_idle_timeout;
    raw_lines;
    refused;
    bad_uri;
    bad_timeout;
    premature_end;
    taxonomy_prints;
  ]
