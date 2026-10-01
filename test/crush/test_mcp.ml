module Config = Crush_core.Config
module Mcp = Crush_core.Mcp
open Lwt_direct

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
  let client = await @@ Mcp.create ~cwd:(Sys.getcwd ()) ~config:stdio_config in
  Fun.protect
    ~finally:(fun () -> await (Mcp.close client))
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
        (List.map (fun (tool : Mcp.tool) -> tool.Mcp.name) tools);
      Alcotest.(check string)
        "tool name" "mcp_fixture_echo"
        (Mcp.tool_name ~server:"fixture" "echo");
      let called =
        or_fail
        @@ await (Mcp.call client ~server:"fixture" ~tool:"echo" ~input:(json_object []))
      in
      Alcotest.(check string) "stdio call" "stdio echo" (Mcp.content_text (fst called));
      Alcotest.(check bool) "stdio call is not an error" false (snd called);
      let listed = or_fail (await (Mcp.resources client ~server:"fixture")) in
      Alcotest.(check int) "stdio resources" 1 (List.length listed);
      let read =
        or_fail @@ await (Mcp.read_resource client ~server:"fixture" ~uri:"fixture://one")
      in
      Alcotest.(check string)
        "stdio resource" "[resource fixture://one]\nstdio resource"
        (Mcp.content_text read);
      let prompts = or_fail (await (Mcp.prompts client ~server:"fixture")) in
      Alcotest.(check int) "stdio prompts" 1 (List.length prompts);
      let prompt =
        or_fail
        @@ await
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

let phrase = function
  | 200 -> "OK"
  | 202 -> "Accepted"
  | 204 -> "No Content"
  | 400 -> "Bad Request"
  | status -> string_of_int status

type http_fixture = { port : int; shutdown : unit -> unit Lwt.t }

let rec read_body flow remaining acc =
  let open Lwt.Syntax in
  if remaining <= 0 then Lwt.return (Buffer.contents acc)
  else
    let* chunk = Lwt_io.read ~count:remaining flow in
    if String.length chunk = 0 then Lwt.return (Buffer.contents acc)
    else begin
      Buffer.add_string acc chunk;
      read_body flow (remaining - String.length chunk) acc
    end

let close_fd_quietly fd =
  Lwt.catch (fun () -> Lwt_unix.close fd) (fun _exn -> Lwt.return_unit)

let start_http ~handle =
  let open Lwt.Syntax in
  let listener = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt listener Lwt_unix.SO_REUSEADDR true;
  await (Lwt_unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)));
  Lwt_unix.listen listener 16;
  let port =
    match Lwt_unix.getsockname listener with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX path -> Alcotest.failf "unexpected Unix listener %s" path
  in
  let running = ref true in
  let read_headers flow =
    let rec loop acc =
      let* line = Lwt_io.read_line flow in
      if line = "" then Lwt.return acc
      else
        match String.index_opt line ':' with
        | Some index ->
            let name = String.lowercase_ascii (String.trim (String.sub line 0 index)) in
            let value =
              String.trim (String.sub line (index + 1) (String.length line - index - 1))
            in
            loop ((name, value) :: acc)
        | None -> loop acc
    in
    loop []
  in
  let respond flow status headers body =
    let head = Buffer.create 256 in
    Buffer.add_string head (Fmt.str "HTTP/1.1 %d %s\r\n" status (phrase status));
    List.iter
      (fun (name, value) -> Buffer.add_string head (Fmt.str "%s: %s\r\n" name value))
      headers;
    Buffer.add_string head
      (Fmt.str "content-length: %d\r\nconnection: close\r\n\r\n" (String.length body));
    let* () = Lwt_io.write flow (Buffer.contents head) in
    let* () = Lwt_io.write flow body in
    Lwt_io.flush flow
  in
  let handle_client client_fd =
    let input = Lwt_io.of_fd ~mode:Lwt_io.input client_fd in
    let output =
      Lwt_io.of_fd ~close:(fun () -> Lwt.return_unit) ~mode:Lwt_io.output client_fd
    in
    Lwt.catch
      (fun () ->
        let* (_request_line : string) = Lwt_io.read_line input in
        let* request_headers = read_headers input in
        let length =
          match List.assoc_opt "content-length" request_headers with
          | Some value -> Option.value (int_of_string_opt value) ~default:0
          | None -> 0
        in
        let* body = read_body input length (Buffer.create 256) in
        let status, headers, response = handle ~request_headers body in
        let* () = respond output status headers response in
        Lwt_io.close input)
      (fun _exn -> close_fd_quietly client_fd)
  in
  let rec accept_loop () =
    if !running then
      Lwt.try_bind
        (fun () -> Lwt_unix.accept listener)
        (fun (client_fd, _addr) ->
          Lwt.async (fun () -> handle_client client_fd);
          accept_loop ())
        (fun exn ->
          match exn with
          | Lwt.Canceled | End_of_file | Unix.Unix_error _ -> Lwt.return_unit
          | exn -> Lwt.fail exn)
    else Lwt.return_unit
  in
  Lwt.async accept_loop;
  {
    port;
    shutdown =
      (fun () ->
        running := false;
        let wake_fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
        let* () =
          Lwt_unix.connect wake_fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port))
        in
        let* () = close_fd_quietly wake_fd in
        close_fd_quietly listener);
  }

let http_fixture session_seen protocol_seen =
  let handle ~request_headers body =
    if List.assoc_opt "mcp-session-id" request_headers = Some "fixture-session" then
      session_seen := true;
    if List.assoc_opt "mcp-protocol-version" request_headers = Some "2025-06-18" then
      protocol_seen := true;
    let request_json =
      match Jsont_bytesrw.decode_string Jsont.json body with
      | Ok value -> value
      | Error message -> Alcotest.failf "fixture request JSON: %s" message
    in
    let method_name = string_member "method" request_json in
    let id = member "id" request_json in
    match (method_name, id) with
    | Some "notifications/initialized", _ -> (202, [], "")
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
        ( 200,
          [ ("content-type", "text/event-stream"); ("mcp-session-id", "fixture-session") ],
          body )
    | _ -> (400, [], "bad request")
  in
  start_http ~handle

let run_http () =
  let session_seen = ref false in
  let protocol_seen = ref false in
  let fixture = http_fixture session_seen protocol_seen in
  Fun.protect
    (fun () ->
      let client =
        await @@ Mcp.create ~cwd:(Sys.getcwd ()) ~config:(http_config fixture.port)
      in
      Fun.protect
        ~finally:(fun () -> await (Mcp.close client))
        (fun () ->
          let state =
            match List.assoc_opt "loopback" (Mcp.states client) with
            | Some value -> value
            | None -> Alcotest.fail "missing loopback state"
          in
          Alcotest.(check bool) "HTTP server connected" true (connected state);
          let call =
            or_fail
            @@ await
                 (Mcp.call client ~server:"loopback" ~tool:"echo" ~input:(json_object []))
          in
          Alcotest.(check string) "HTTP call" "http echo" (Mcp.content_text (fst call));
          Alcotest.(check bool) "HTTP call is not an error" false (snd call);
          let resources = or_fail (await (Mcp.resources client ~server:"loopback")) in
          Alcotest.(check int) "HTTP resources" 1 (List.length resources);
          let content =
            or_fail
            @@ await (Mcp.read_resource client ~server:"loopback" ~uri:"loopback://one")
          in
          Alcotest.(check string)
            "HTTP resource" "[resource loopback://one]\nhttp resource"
            (Mcp.content_text content);
          let prompts = or_fail (await (Mcp.prompts client ~server:"loopback")) in
          Alcotest.(check int) "HTTP prompts" 1 (List.length prompts);
          let prompt =
            or_fail
            @@ await (Mcp.get_prompt client ~server:"loopback" ~name:"hello" ~args:[])
          in
          Alcotest.(check string) "HTTP prompt" "http prompt" prompt;
          Alcotest.(check bool) "session header echoed" true !session_seen;
          Alcotest.(check bool) "protocol header sent" true !protocol_seen))
    ~finally:(fun () -> await (fixture.shutdown ()))

let server_request_id = 424_242

let json_as_int = function
  | Jsont.Number (number, _) when Float.is_integer number -> Some (int_of_float number)
  | _ -> None

let http_server_request_fixture ~session_id ~protocol_version =
  let replied, replied_resolver = Lwt.wait () in
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
  let handle ~request_headers body =
    let request_json =
      match Jsont_bytesrw.decode_string Jsont.json body with
      | Ok value -> value
      | Error message -> Alcotest.failf "fixture request JSON: %s" message
    in
    let method_name = string_member "method" request_json in
    let id = member "id" request_json in
    match (method_name, id) with
    | Some "notifications/initialized", _ -> (202, [], "")
    | Some "initialize", Some id ->
        let result =
          json_object
            [
              ("protocolVersion", json_string protocol_version);
              ("capabilities", json_object []);
            ]
        in
        ( 200,
          [ ("content-type", "text/event-stream"); ("mcp-session-id", session_id) ],
          sse_body [ http_result id result ] )
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
        ( 200,
          [ ("content-type", "text/event-stream") ],
          sse_body [ request_event; answer_event ] )
    | Some (("resources/list" | "prompts/list") as list_method), Some id ->
        let field = if list_method = "resources/list" then "resources" else "prompts" in
        ( 200,
          [ ("content-type", "text/event-stream") ],
          sse_body [ http_result id (json_object [ (field, json_list []) ]) ] )
    | None, Some id_value ->
        if json_as_int id_value = Some server_request_id then begin
          correlation_id_seen := true;
          if List.assoc_opt "mcp-session-id" request_headers = Some session_id then
            reply_session_seen := true;
          if List.assoc_opt "mcp-protocol-version" request_headers = Some protocol_version
          then reply_protocol_seen := true;
          Lwt.wakeup_later replied_resolver ()
        end;
        (202, [], "")
    | _ -> (400, [], "bad request")
  in
  let fixture = start_http ~handle in
  (fixture, replied, correlation_id_seen, reply_session_seen, reply_protocol_seen)

let run_http_server_request () =
  let session_id = "server-request-session" in
  let protocol_version = "2024-11-05" in
  let fixture, replied, correlation_id_seen, reply_session_seen, reply_protocol_seen =
    http_server_request_fixture ~session_id ~protocol_version
  in
  Fun.protect
    (fun () ->
      let client =
        await @@ Mcp.create ~cwd:(Sys.getcwd ()) ~config:(http_config fixture.port)
      in
      Fun.protect
        ~finally:(fun () -> await (Mcp.close client))
        (fun () ->
          (match
             await
             @@ Lwt.catch
                  (fun () ->
                    Lwt.map
                      (fun () -> Ok ())
                      (Lwt_unix.with_timeout 5. (fun () -> replied)))
                  (function
                    | Lwt_unix.Timeout -> Lwt.return (Error `Timeout)
                    | exn -> Lwt.fail exn)
           with
          | Ok () -> ()
          | Error `Timeout ->
              Alcotest.fail "server-initiated MCP request was never answered");
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
            (connected state)))
    ~finally:(fun () -> await (fixture.shutdown ()))

let cases =
  [
    Test_tools_test_support.case "stdio JSONL MCP" `Quick run_stdio;
    Test_tools_test_support.case "streamable HTTP SSE MCP" `Quick run_http;
    Test_tools_test_support.case "streamable HTTP server-initiated request/reply" `Quick
      run_http_server_request;
  ]
