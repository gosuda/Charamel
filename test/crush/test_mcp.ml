module Config = Crush_core.Config
module Mcp = Crush_core.Mcp

let member name value =
  match value with
  | Jsont.Object (members, _) -> Option.map snd (Jsont.Json.find_mem name members)
  | _ -> None

let string_member name value =
  match member name value with Some (Jsont.String (value, _)) -> Some value | _ -> None

let json_string value = Jsont.Json.string value

let json_object fields =
  fields
  |> List.map (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
  |> Jsont.Json.object'

let json_list values = Jsont.Json.list values

let or_fail = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%a" Mcp.pp_error error

let fixture_env = "CRUSH_MCP_OCAML_FIXTURE"

let fixture_response request =
  let id = member "id" request in
  match (string_member "method" request, id) with
  | Some "notifications/initialized", _ -> None
  | Some "initialize", Some id ->
      Some
        (json_object
           [
             ("jsonrpc", json_string "2.0");
             ("id", id);
             ( "result",
               json_object
                 [
                   ("protocolVersion", json_string "2025-06-18");
                   ("capabilities", json_object []);
                 ] );
           ])
  | Some "tools/list", Some id ->
      Some
        (json_object
           [
             ("jsonrpc", json_string "2.0");
             ("id", id);
             ( "result",
               json_object
                 [
                   ( "tools",
                     json_list
                       [
                         json_object
                           [
                             ("name", json_string "echo");
                             ("description", json_string "Echo");
                             ( "inputSchema",
                               json_object [ ("type", json_string "object") ] );
                           ];
                       ] );
                 ] );
           ])
  | Some "resources/list", Some id ->
      Some
        (json_object
           [
             ("jsonrpc", json_string "2.0");
             ("id", id);
             ( "result",
               json_object
                 [
                   ( "resources",
                     json_list
                       [
                         json_object
                           [
                             ("uri", json_string "fixture://one");
                             ("name", json_string "one");
                             ("mimeType", json_string "text/plain");
                           ];
                       ] );
                 ] );
           ])
  | Some "prompts/list", Some id ->
      Some
        (json_object
           [
             ("jsonrpc", json_string "2.0");
             ("id", id);
             ( "result",
               json_object
                 [
                   ( "prompts",
                     json_list
                       [
                         json_object
                           [
                             ("name", json_string "hello");
                             ("description", json_string "Hello");
                             ( "arguments",
                               json_list
                                 [
                                   json_object
                                     [
                                       ("name", json_string "name");
                                       ("required", Jsont.Json.bool true);
                                     ];
                                 ] );
                           ];
                       ] );
                 ] );
           ])
  | Some "tools/call", Some id ->
      Some
        (json_object
           [
             ("jsonrpc", json_string "2.0");
             ("id", id);
             ( "result",
               json_object
                 [
                   ( "content",
                     json_list
                       [
                         json_object
                           [
                             ("type", json_string "text");
                             ("text", json_string "stdio echo");
                           ];
                       ] );
                   ("isError", Jsont.Json.bool false);
                 ] );
           ])
  | Some "resources/read", Some id ->
      Some
        (json_object
           [
             ("jsonrpc", json_string "2.0");
             ("id", id);
             ( "result",
               json_object
                 [
                   ( "contents",
                     json_list
                       [
                         json_object
                           [
                             ("uri", json_string "fixture://one");
                             ("mimeType", json_string "text/plain");
                             ("text", json_string "stdio resource");
                           ];
                       ] );
                 ] );
           ])
  | Some "prompts/get", Some id ->
      Some
        (json_object
           [
             ("jsonrpc", json_string "2.0");
             ("id", id);
             ( "result",
               json_object
                 [
                   ( "messages",
                     json_list
                       [
                         json_object
                           [
                             ("role", json_string "user");
                             ( "content",
                               json_object
                                 [
                                   ("type", json_string "text");
                                   ("text", json_string "stdio prompt");
                                 ] );
                           ];
                       ] );
                 ] );
           ])
  | Some method_name, Some id ->
      Some
        (json_object
           [
             ("jsonrpc", json_string "2.0");
             ("id", id);
             ( "error",
               json_object
                 [
                   ("code", Jsont.Json.int (-32_601));
                   ("message", json_string ("unknown fixture method " ^ method_name));
                 ] );
           ])
  | _ -> None

let fixture_loop () =
  let rec loop () =
    match input_line Stdlib.stdin with
    | exception End_of_file -> ()
    | line ->
        (match Jsont_bytesrw.decode_string Jsont.json line with
        | Error _ -> ()
        | Ok request -> (
            match fixture_response request with
            | None -> ()
            | Some response -> (
                match Jsont_bytesrw.encode_string Jsont.json response with
                | Ok encoded ->
                    output_string Stdlib.stdout encoded;
                    output_char Stdlib.stdout '\n';
                    flush Stdlib.stdout
                | Error _ -> ())));
        loop ()
  in
  loop ()

let () =
  match Sys.getenv_opt fixture_env with
  | Some "1" ->
      fixture_loop ();
      exit 0
  | _ -> ()

let stdio_config =
  let server : Config.mcp =
    {
      transport = Config.Stdio;
      command = Some Sys.executable_name;
      args = [];
      env = [ (fixture_env, "1") ];
      url = None;
      headers = [];
      timeout_s = 2;
    }
  in
  { Config.default with mcp = [ ("fixture", server) ] }

let connected server = match server with Mcp.Connected _ -> true | _ -> false

let run_stdio () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    Mcp.create ~sw ~proc_mgr:env#process_mgr ~net:env#net ~clock:env#clock
      ~cwd:(Sys.getcwd ()) ~config:stdio_config
  in
  Fun.protect
    ~finally:(fun () -> Mcp.close client)
    (fun () ->
      let states = Mcp.states client in
      Alcotest.(check bool)
        "stdio server connected" true
        (match List.assoc_opt "fixture" states with
        | Some state -> connected state
        | None -> false);
      let tools = Mcp.tools client in
      Alcotest.(check (list string))
        "stdio tool names" [ "echo" ]
        (List.map (fun (tool : Mcp.tool) -> tool.name) tools);
      Alcotest.(check string)
        "tool name" "mcp_fixture_echo"
        (Mcp.tool_name ~server:"fixture" "echo");
      let called =
        or_fail (Mcp.call client ~server:"fixture" ~tool:"echo" ~input:(json_object []))
      in
      Alcotest.(check string) "stdio call" "stdio echo" (Mcp.content_text (fst called));
      Alcotest.(check bool) "stdio call is not an error" false (snd called);
      let listed = or_fail (Mcp.resources client ~server:"fixture") in
      Alcotest.(check int) "stdio resources" 1 (List.length listed);
      let read =
        or_fail (Mcp.read_resource client ~server:"fixture" ~uri:"fixture://one")
      in
      Alcotest.(check string)
        "stdio resource" "[resource fixture://one]\nstdio resource"
        (Mcp.content_text read);
      let prompts = or_fail (Mcp.prompts client ~server:"fixture") in
      Alcotest.(check int) "stdio prompts" 1 (List.length prompts);
      let prompt =
        or_fail
          (Mcp.get_prompt client ~server:"fixture" ~name:"hello"
             ~args:[ ("name", "Ada") ])
      in
      Alcotest.(check string) "stdio prompt" "stdio prompt" prompt)

let http_config port =
  let server : Config.mcp =
    {
      transport = Config.Http;
      command = None;
      args = [];
      env = [];
      url = Some (Fmt.str "http://127.0.0.1:%d/mcp" port);
      headers = [];
      timeout_s = 2;
    }
  in
  { Config.default with mcp = [ ("loopback", server) ] }

let http_result id result =
  json_object [ ("jsonrpc", json_string "2.0"); ("id", id); ("result", result) ]

let http_fixture env sw session_seen protocol_seen =
  let listener =
    Eio.Net.listen ~reuse_addr:true ~backlog:16 ~sw env#net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | `Unix path -> Alcotest.failf "unexpected Unix listener %s" path
  in
  let callback _conn request body =
    let headers = Http.Request.headers request in
    if Http.Header.get headers "mcp-session-id" = Some "fixture-session" then
      session_seen := true;
    if Http.Header.get headers "mcp-protocol-version" = Some "2025-06-18" then
      protocol_seen := true;
    let raw =
      Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:(16 * 1024 * 1024) body)
    in
    let request_json =
      match Jsont_bytesrw.decode_string Jsont.json raw with
      | Ok value -> value
      | Error message -> Alcotest.failf "fixture request JSON: %s" message
    in
    let method_name = string_member "method" request_json in
    let id = member "id" request_json in
    match (method_name, id) with
    | Some "notifications/initialized", _ ->
        Cohttp_eio.Server.respond_string ~status:`No_content ~body:"" ()
    | Some method_name, Some id ->
        let result =
          match method_name with
          | "initialize" ->
              json_object
                [
                  ("protocolVersion", json_string "2025-06-18");
                  ("capabilities", json_object []);
                  ("serverInfo", json_object [ ("name", json_string "loopback") ]);
                ]
          | "tools/list" ->
              json_object
                [
                  ( "tools",
                    json_list
                      [
                        json_object
                          [
                            ("name", json_string "echo");
                            ("description", json_string "Echo");
                            ("inputSchema", json_object [ ("type", json_string "object") ]);
                          ];
                      ] );
                ]
          | "resources/list" ->
              json_object
                [
                  ( "resources",
                    json_list
                      [
                        json_object
                          [
                            ("uri", json_string "loopback://one");
                            ("name", json_string "one");
                            ("mimeType", json_string "text/plain");
                          ];
                      ] );
                ]
          | "prompts/list" ->
              json_object
                [
                  ( "prompts",
                    json_list
                      [
                        json_object
                          [
                            ("name", json_string "hello");
                            ("description", json_string "Hello");
                            ("arguments", json_list []);
                          ];
                      ] );
                ]
          | "tools/call" ->
              json_object
                [
                  ( "content",
                    json_list
                      [
                        json_object
                          [
                            ("type", json_string "text"); ("text", json_string "http echo");
                          ];
                      ] );
                  ("isError", Jsont.Json.bool false);
                ]
          | "resources/read" ->
              json_object
                [
                  ( "contents",
                    json_list
                      [
                        json_object
                          [
                            ("uri", json_string "loopback://one");
                            ("mimeType", json_string "text/plain");
                            ("text", json_string "http resource");
                          ];
                      ] );
                ]
          | "prompts/get" ->
              json_object
                [
                  ( "messages",
                    json_list
                      [
                        json_object
                          [
                            ("role", json_string "user");
                            ( "content",
                              json_object
                                [
                                  ("type", json_string "text");
                                  ("text", json_string "http prompt");
                                ] );
                          ];
                      ] );
                ]
          | _ -> json_object []
        in
        let response = http_result id result in
        let body =
          match Jsont_bytesrw.encode_string Jsont.json response with
          | Ok encoded -> "data: " ^ encoded ^ "\n\n"
          | Error message -> Alcotest.failf "fixture response JSON: %s" message
        in
        let response_headers =
          Http.Header.of_list
            [
              ("content-type", "text/event-stream"); ("mcp-session-id", "fixture-session");
            ]
        in
        Cohttp_eio.Server.respond_string ~headers:response_headers ~status:`OK ~body ()
    | _ -> Cohttp_eio.Server.respond_string ~status:`Bad_request ~body:"bad request" ()
  in
  let server = Cohttp_eio.Server.make ~callback () in
  let stop, stop_resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run ~stop ~on_error:(fun _ -> ()) listener server);
  (port, stop_resolver)

let run_http () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let session_seen = ref false in
  let protocol_seen = ref false in
  let port, stop_resolver = http_fixture env sw session_seen protocol_seen in
  let client =
    Mcp.create ~sw ~proc_mgr:env#process_mgr ~net:env#net ~clock:env#clock
      ~cwd:(Sys.getcwd ()) ~config:(http_config port)
  in
  Fun.protect
    ~finally:(fun () ->
      Mcp.close client;
      ignore (Eio.Promise.try_resolve stop_resolver ()))
    (fun () ->
      let state =
        match List.assoc_opt "loopback" (Mcp.states client) with
        | Some value -> value
        | None -> Alcotest.fail "missing loopback state"
      in
      Alcotest.(check bool) "HTTP server connected" true (connected state);
      let call =
        or_fail (Mcp.call client ~server:"loopback" ~tool:"echo" ~input:(json_object []))
      in
      Alcotest.(check string) "HTTP call" "http echo" (Mcp.content_text (fst call));
      Alcotest.(check bool) "HTTP call is not an error" false (snd call);
      let resources = or_fail (Mcp.resources client ~server:"loopback") in
      Alcotest.(check int) "HTTP resources" 1 (List.length resources);
      let content =
        or_fail (Mcp.read_resource client ~server:"loopback" ~uri:"loopback://one")
      in
      Alcotest.(check string)
        "HTTP resource" "[resource loopback://one]\nhttp resource"
        (Mcp.content_text content);
      let prompts = or_fail (Mcp.prompts client ~server:"loopback") in
      Alcotest.(check int) "HTTP prompts" 1 (List.length prompts);
      let prompt =
        or_fail (Mcp.get_prompt client ~server:"loopback" ~name:"hello" ~args:[])
      in
      Alcotest.(check string) "HTTP prompt" "http prompt" prompt;
      Alcotest.(check bool) "session header echoed" true !session_seen;
      Alcotest.(check bool) "protocol header sent" true !protocol_seen)

let server_request_id = 424_242

let json_as_int = function
  | Jsont.Number (number, _) when Float.is_integer number -> Some (int_of_float number)
  | _ -> None

let http_server_request_fixture env sw ~session_id ~protocol_version =
  let listener =
    Eio.Net.listen ~reuse_addr:true ~backlog:16 ~sw env#net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | `Unix path -> Alcotest.failf "unexpected Unix listener %s" path
  in
  let replied, replied_resolver = Eio.Promise.create () in
  let correlation_id_seen = ref false in
  let reply_session_seen = ref false in
  let reply_protocol_seen = ref false in
  let encode value =
    match Jsont_bytesrw.encode_string Jsont.json value with
    | Ok encoded -> encoded
    | Error message -> Alcotest.failf "fixture response JSON: %s" message
  in
  let sse_body events =
    String.concat "" (List.map (fun value -> "data: " ^ encode value ^ "\n\n") events)
  in
  let callback _conn request body =
    let headers = Http.Request.headers request in
    let raw =
      Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:(16 * 1024 * 1024) body)
    in
    let request_json =
      match Jsont_bytesrw.decode_string Jsont.json raw with
      | Ok value -> value
      | Error message -> Alcotest.failf "fixture request JSON: %s" message
    in
    let method_name = string_member "method" request_json in
    let id = member "id" request_json in
    match (method_name, id) with
    | Some "notifications/initialized", _ ->
        Cohttp_eio.Server.respond_string ~status:`No_content ~body:"" ()
    | Some "initialize", Some id ->
        let result =
          json_object
            [
              ("protocolVersion", json_string protocol_version);
              ("capabilities", json_object []);
            ]
        in
        let response_headers =
          Http.Header.of_list
            [ ("content-type", "text/event-stream"); ("mcp-session-id", session_id) ]
        in
        Cohttp_eio.Server.respond_string ~headers:response_headers ~status:`OK
          ~body:(sse_body [ http_result id result ])
          ()
    | Some "tools/list", Some id ->
        let request_event =
          json_object
            [
              ("jsonrpc", json_string "2.0");
              ("id", Jsont.Json.int server_request_id);
              ("method", json_string "ping");
              ("params", json_object []);
            ]
        in
        let answer_event = http_result id (json_object [ ("tools", json_list []) ]) in
        let response_headers =
          Http.Header.of_list [ ("content-type", "text/event-stream") ]
        in
        Cohttp_eio.Server.respond_string ~headers:response_headers ~status:`OK
          ~body:(sse_body [ request_event; answer_event ])
          ()
    | Some (("resources/list" | "prompts/list") as list_method), Some id ->
        let field = if list_method = "resources/list" then "resources" else "prompts" in
        let response_headers =
          Http.Header.of_list [ ("content-type", "text/event-stream") ]
        in
        Cohttp_eio.Server.respond_string ~headers:response_headers ~status:`OK
          ~body:(sse_body [ http_result id (json_object [ (field, json_list []) ]) ])
          ()
    | None, Some id_value ->
        if json_as_int id_value = Some server_request_id then (
          correlation_id_seen := true;
          if Http.Header.get headers "mcp-session-id" = Some session_id then
            reply_session_seen := true;
          if Http.Header.get headers "mcp-protocol-version" = Some protocol_version then
            reply_protocol_seen := true;
          ignore (Eio.Promise.try_resolve replied_resolver ()));
        Cohttp_eio.Server.respond_string ~status:`Accepted ~body:"" ()
    | _ -> Cohttp_eio.Server.respond_string ~status:`Bad_request ~body:"bad request" ()
  in
  let server = Cohttp_eio.Server.make ~callback () in
  let stop, stop_resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run ~stop ~on_error:(fun _ -> ()) listener server);
  ( port,
    stop_resolver,
    replied,
    correlation_id_seen,
    reply_session_seen,
    reply_protocol_seen )

let run_http_server_request () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let session_id = "server-request-session" in
  let protocol_version = "2024-11-05" in
  let ( port,
        stop_resolver,
        replied,
        correlation_id_seen,
        reply_session_seen,
        reply_protocol_seen ) =
    http_server_request_fixture env sw ~session_id ~protocol_version
  in
  let client =
    Mcp.create ~sw ~proc_mgr:env#process_mgr ~net:env#net ~clock:env#clock
      ~cwd:(Sys.getcwd ()) ~config:(http_config port)
  in
  Fun.protect
    ~finally:(fun () ->
      Mcp.close client;
      ignore (Eio.Promise.try_resolve stop_resolver ()))
    (fun () ->
      (match
         Eio.Time.with_timeout env#clock 5. (fun () -> Ok (Eio.Promise.await replied))
       with
      | Ok () -> ()
      | Error `Timeout -> Alcotest.fail "server-initiated MCP request was never answered");
      Alcotest.(check bool)
        "server request correlation id echoed" true !correlation_id_seen;
      Alcotest.(check bool)
        "server request reply carried session header" true !reply_session_seen;
      Alcotest.(check bool)
        "server request reply carried protocol header" true !reply_protocol_seen;
      let state =
        match List.assoc_opt "loopback" (Mcp.states client) with
        | Some value -> value
        | None -> Alcotest.fail "missing loopback state"
      in
      Alcotest.(check bool)
        "HTTP client stayed connected after server-initiated request" true
        (connected state))

let cases =
  [
    Alcotest.test_case "stdio JSONL MCP" `Quick run_stdio;
    Alcotest.test_case "streamable HTTP SSE MCP" `Quick run_http;
    Alcotest.test_case "streamable HTTP server-initiated request/reply" `Quick
      run_http_server_request;
  ]
