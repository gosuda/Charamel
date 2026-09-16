open Charm_fantasy

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

let provider server =
  Provider.anthropic
    ~base_url:(Fixture_server.base_url server)
    ~auth:(Provider.Api_key "fixture-key") ()

let drain stream =
  let rec loop acc =
    match Eio.Stream.take stream with
    | Stream_part.Finish _ as terminal -> List.rev (terminal :: acc)
    | part -> loop (part :: acc)
  in
  loop []

let run body f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = Fixture_server.start ~sw ~net:env#net () in
  body server;
  f env sw server

let test_split_sse () =
  run
    (fun server -> Fixture_server.respond server split_body)
    (fun env sw server ->
      let parts =
        Provider.stream (provider server) ~sw ~clock:env#clock ~net:env#net ~model
          [ user_message ]
        |> drain
      in
      Alcotest.(check bool)
        "multiline SSE data reaches codec" true
        (List.exists
           (function Stream_part.Text_delta "split" -> true | _ -> false)
           parts);
      Alcotest.(check int) "one request" 1 (Fixture_server.request_count server))

let test_headers () =
  run
    (fun server -> Fixture_server.respond server complete_body)
    (fun env sw server ->
      ignore
        (Provider.stream (provider server) ~sw ~clock:env#clock ~net:env#net ~model
           [ user_message ]
        |> drain);
      let headers = Fixture_server.last_headers server in
      let has name value =
        List.exists
          (fun (key, actual) ->
            String.equal key name
            &&
            match value with
            | None -> true
            | Some expected -> String.equal actual expected)
          headers
      in
      Alcotest.(check bool)
        "Anthropic version header" true
        (has "anthropic-version" (Some "2023-06-01"));
      Alcotest.(check bool) "API key header" true (has "x-api-key" (Some "fixture-key")))

let test_retry_after () =
  run
    (fun server ->
      Fixture_server.respond_with server ~status:429 ~retry_after:0.1 ~headers:[] "busy";
      Fixture_server.respond server complete_body)
    (fun env sw server ->
      let started = Eio.Time.now env#clock in
      let parts =
        Provider.stream (provider server) ~sw ~clock:env#clock ~net:env#net ~model
          [ user_message ]
        |> drain
      in
      let elapsed = Eio.Time.now env#clock -. started in
      Alcotest.(check bool) "retry-after delay is observed" true (elapsed >= 0.08);
      Alcotest.(check int)
        "retry makes one follow-up request" 2
        (Fixture_server.request_count server);
      Alcotest.(check bool)
        "retry ends successfully" true
        (List.exists (function Stream_part.Finish `Stop -> true | _ -> false) parts))

let test_no_retry_after_emission () =
  let partial =
    {|event: message_start
data: {"type":"message_start","message":{}}

event: content_block_start
data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"partial"}}

|}
  in
  run
    (fun server -> Fixture_server.respond server partial)
    (fun env sw server ->
      let parts =
        Provider.stream (provider server) ~sw ~clock:env#clock ~net:env#net ~model
          [ user_message ]
        |> drain
      in
      Alcotest.(check int)
        "emitted stream is not retried" 1
        (Fixture_server.request_count server);
      Alcotest.(check bool)
        "premature EOF is terminal error" true
        (match List.rev parts with
        | Stream_part.Finish (`Error _) :: _ -> true
        | _ -> false))

let test_malformed_body () =
  run
    (fun server ->
      Fixture_server.respond server "event: message_start\ndata: not-json\n\n")
    (fun env sw server ->
      let parts =
        Provider.stream (provider server) ~sw ~clock:env#clock ~net:env#net ~model
          [ user_message ]
        |> drain
      in
      Alcotest.(check bool)
        "malformed SSE is an error finish" true
        (match parts with [ Stream_part.Finish (`Error _) ] -> true | _ -> false))

let test_deadline_cancellation () =
  run
    (fun server ->
      Fixture_server.respond_with server ~status:429 ~retry_after:30. ~headers:[] "busy")
    (fun env _sw server ->
      let result =
        try
          Eio.Switch.run @@ fun call_sw ->
          let stream =
            Provider.stream (provider server) ~sw:call_sw ~clock:env#clock ~net:env#net
              ~model [ user_message ]
          in
          Eio.Fiber.fork ~sw:call_sw (fun () ->
              Eio.Time.sleep env#clock 0.05;
              Eio.Switch.fail call_sw Eio.Time.Timeout);
          ignore (Eio.Stream.take stream);
          Ok ()
        with Eio.Time.Timeout -> Error `Timeout
      in
      Alcotest.(check bool)
        "caller deadline cancels retry wait" true
        (match result with Error `Timeout -> true | Ok () -> false);
      Alcotest.(check int)
        "deadline test starts one request" 1
        (Fixture_server.request_count server))

let cases =
  [
    Alcotest.test_case "split SSE data" `Quick test_split_sse;
    Alcotest.test_case "provider headers" `Quick test_headers;
    Alcotest.test_case "Retry-After" `Quick test_retry_after;
    Alcotest.test_case "no retry after emission" `Quick test_no_retry_after_emission;
    Alcotest.test_case "malformed body" `Quick test_malformed_body;
    Alcotest.test_case "deadline cancellation" `Quick test_deadline_cancellation;
  ]
