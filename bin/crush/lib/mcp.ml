type error =
  [ `Unknown_server of string
  | `Not_connected of string
  | `Rpc of string * int * string
  | `Timeout of string
  | `Transport of string * string ]

type tool = { server : string; name : string; description : string; schema : Jsont.json }

type resource = {
  server : string;
  uri : string;
  name : string;
  mime : string option;
  description : string option;
}

type prompt = {
  server : string;
  name : string;
  description : string;
  arguments : (string * bool) list;
}

type content =
  | Text of string
  | Image of { mime : string; data : string }
  | Resource of { uri : string; text : string option }

type state =
  | Connecting
  | Connected of { tools : int; resources : int; prompts : int }
  | Failed of string
  | Disabled

let pp_error ppf = function
  | `Unknown_server server -> Fmt.pf ppf "unknown MCP server %s" server
  | `Not_connected server -> Fmt.pf ppf "MCP server %s is not connected" server
  | `Rpc (server, code, message) ->
      Fmt.pf ppf "MCP server %s returned JSON-RPC %d: %s" server code message
  | `Timeout server -> Fmt.pf ppf "MCP server %s request timed out" server
  | `Transport (server, message) ->
      Fmt.pf ppf "MCP server %s transport: %s" server message

module Log = struct
  let src = Logs.Src.create "crush.mcp"

  module T = (val Logs.src_log src : Logs.LOG)
end

let max_message_bytes = 16 * 1024 * 1024
let max_http_body_bytes = 16 * 1024 * 1024
let max_pages = 1_024
let readiness_timeout = 10.
let protocol_version = "2025-06-18"

open Result.Syntax

let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.int value
let json_list values = Jsont.Json.list values

let json_object members =
  members
  |> List.map (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
  |> Jsont.Json.object'

let array_member name value =
  match Jsonx.member name value with
  | Some (Jsont.Array (values, _)) -> Some values
  | _ -> None

let object_member name value =
  match Jsonx.member name value with
  | Some (Jsont.Object _ as value) -> Some value
  | _ -> None

let error_message = function
  | `Unknown_server server -> Fmt.str "unknown MCP server %s" server
  | `Not_connected server -> Fmt.str "MCP server %s is not connected" server
  | `Rpc (_, code, message) -> Fmt.str "JSON-RPC %d: %s" code message
  | `Timeout _server -> Fmt.str "request timed out after %g seconds" readiness_timeout
  | `Transport (_, message) -> message

type pending = {
  promise : (Jsont.json, error) result Eio.Promise.t;
  resolver : (Jsont.json, error) result Eio.Promise.u;
}

type stdio = {
  process : Eio_unix.Process.ty Eio.Resource.t;
  input : [ Eio.Flow.sink_ty | Eio.Resource.close_ty ] Eio.Resource.t;
  output : [ Eio.Flow.source_ty | Eio.Resource.close_ty ] Eio.Resource.t;
  reader : Eio.Buf_read.t;
  writer_lock : Eio.Mutex.t;
}

type http = {
  client : Cohttp_eio.Client.t;
  uri : Uri.t;
  headers : (string * string) list;
  mutable session_id : string option;
  mutable protocol_version : string;
  session_lock : Eio.Mutex.t;
  writer_lock : Eio.Mutex.t;
  reply_queue : Jsont.json Eio.Stream.t;
  stopped : unit Eio.Promise.t;
  stop : unit Eio.Promise.u;
  mutable closed : bool;
}

type transport = Stdio of stdio | Http_transport of http

type server = {
  name : string;
  config : Config.mcp;
  sw : Eio.Switch.t;
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  net : Eio_unix.Net.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  cwd : string;
  lock : Eio.Mutex.t;
  mutable state : state;
  mutable transport : transport option;
  mutable next_id : int;
  pending : (int, pending) Hashtbl.t;
  ready : (unit, string) result Eio.Promise.t;
  ready_resolver : (unit, string) result Eio.Promise.u;
  mutable closed : bool;
  mutable tools : tool list;
}

type t = { lock : Eio.Mutex.t; mutable closed : bool; servers : server list }

let status_is_connected = function Connected _ -> true | _ -> false
let status (server : server) = Eio.Mutex.use_ro server.lock (fun () -> server.state)

let server_transport (server : server) =
  Eio.Mutex.use_ro server.lock (fun () ->
      if server.closed then None else server.transport)

let close_flow flow = Eio.Cancel.protect (fun () -> Eio.Flow.close flow)

let cleanup_transport (server : server) = function
  | Stdio stdio -> (
      (try close_flow stdio.input with Eio.Io _ -> ());
      (try close_flow stdio.output with Eio.Io _ -> ());
      Eio.Process.signal stdio.process Sys.sigterm;
      match
        Eio.Time.with_timeout server.clock 2. (fun () ->
            ignore (Eio.Process.await stdio.process);
            Ok ())
      with
      | Ok _ -> ()
      | Error `Timeout ->
          Eio.Process.signal stdio.process Sys.sigkill;
          ignore
            (Eio.Time.with_timeout server.clock 1. (fun () ->
                 ignore (Eio.Process.await stdio.process);
                 Ok ())))
  | Http_transport http ->
      http.closed <- true;
      ignore (Eio.Promise.try_resolve http.stop ())

let reject_pending (server : server) error =
  let pending =
    Eio.Mutex.use_rw ~protect:true server.lock (fun () ->
        let values = Hashtbl.fold (fun _ value acc -> value :: acc) server.pending [] in
        Hashtbl.clear server.pending;
        values)
  in
  List.iter
    (fun item -> ignore (Eio.Promise.try_resolve item.resolver (Error error)))
    pending

let fail_server (server : server) message =
  let transport =
    Eio.Mutex.use_rw ~protect:true server.lock (fun () ->
        if server.closed || server.state = Disabled then None
        else (
          server.closed <- true;
          server.state <- Failed message;
          ignore (Eio.Promise.try_resolve server.ready_resolver (Error message));
          let transport = server.transport in
          server.transport <- None;
          transport))
  in
  reject_pending server (`Transport (server.name, message));
  Option.iter
    (fun transport ->
      Eio.Fiber.fork ~sw:server.sw (fun () ->
          try cleanup_transport server transport with Eio.Cancel.Cancelled _ -> ()))
    transport

let connect_server (server : server) =
  Eio.Mutex.use_rw ~protect:true server.lock (fun () ->
      if server.closed || server.state <> Connecting then false
      else (
        server.state <- Connecting;
        true))

let install_transport (server : server) transport =
  let accepted =
    Eio.Mutex.use_rw ~protect:true server.lock (fun () ->
        if server.closed then false
        else (
          server.transport <- Some transport;
          true))
  in
  if not accepted then cleanup_transport server transport;
  accepted

let close_server (server : server) =
  let transport =
    Eio.Mutex.use_rw ~protect:true server.lock (fun () ->
        if server.closed then (
          server.state <- Disabled;
          None)
        else (
          server.closed <- true;
          server.state <- Disabled;
          ignore (Eio.Promise.try_resolve server.ready_resolver (Error "client closed"));
          let values = Hashtbl.fold (fun _ value acc -> value :: acc) server.pending [] in
          Hashtbl.clear server.pending;
          List.iter
            (fun item ->
              ignore
                (Eio.Promise.try_resolve item.resolver
                   (Error (`Transport (server.name, "client closed")))))
            values;
          let transport = server.transport in
          server.transport <- None;
          transport))
  in
  Option.iter (cleanup_transport server) transport

let response_error server value =
  match object_member "error" value with
  | Some error ->
      let code = Option.value (Jsonx.int_member "code" error) ~default:(-32_000) in
      let message =
        Option.value
          (Jsonx.string_member "message" error)
          ~default:"malformed JSON-RPC error"
      in
      Error (`Rpc (server.name, code, message))
  | None -> (
      match Jsonx.member "result" value with
      | Some result -> Ok result
      | None ->
          Error (`Transport (server.name, "JSON-RPC response has no result or error")))

let id_as_int value =
  match value with
  | Jsont.Number (number, _) when Float.is_integer number ->
      if number >= float_of_int min_int && number <= float_of_int max_int then
        Some (int_of_float number)
      else None
  | _ -> None

let id_member value =
  match Jsonx.member "id" value with
  | Some (Jsont.Null _) | None -> None
  | Some value -> Some value

let request_json id method_name params =
  json_object
    [
      ("jsonrpc", json_string "2.0");
      ("id", json_int id);
      ("method", json_string method_name);
      ("params", params);
    ]

let notification_json method_name params =
  json_object
    [
      ("jsonrpc", json_string "2.0");
      ("method", json_string method_name);
      ("params", params);
    ]

let response_json id result =
  json_object [ ("jsonrpc", json_string "2.0"); ("id", id); ("result", result) ]

let error_response_json id code message =
  json_object
    [
      ("jsonrpc", json_string "2.0");
      ("id", id);
      ("error", json_object [ ("code", json_int code); ("message", json_string message) ]);
    ]

let server_reply method_name id =
  match method_name with
  | "ping" -> response_json id (json_object [])
  | "roots/list" -> response_json id (json_object [ ("roots", json_list []) ])
  | _ -> error_response_json id (-32_601) "method not found"

let timeout_seconds server = max 0. (float_of_int server.config.Config.timeout_s)

let write_stdio (server : server) (stdio : stdio) value =
  let line = Jsonx.string_of_json value ^ "\n" in
  match
    Eio.Time.with_timeout server.clock (timeout_seconds server) (fun () ->
        Eio.Mutex.lock stdio.writer_lock;
        Fun.protect
          ~finally:(fun () -> Eio.Mutex.unlock stdio.writer_lock)
          (fun () ->
            Eio.Flow.copy_string line stdio.input;
            Ok ()))
  with
  | Ok () -> Ok ()
  | Error `Timeout -> Error (`Timeout server.name)

let resolve_pending (server : server) id result =
  let pending =
    Eio.Mutex.use_rw ~protect:true server.lock (fun () ->
        let pending = Hashtbl.find_opt server.pending id in
        Option.iter (fun _ -> Hashtbl.remove server.pending id) pending;
        pending)
  in
  match pending with
  | None ->
      Log.T.debug (fun log ->
          log "ignoring late or unknown response from %s (id %d)" server.name id)
  | Some pending -> ignore (Eio.Promise.try_resolve pending.resolver result)

let dispatch_stdio server stdio value =
  match value with
  | Jsont.Object _ when Jsonx.string_member "jsonrpc" value = Some "2.0" -> (
      match Jsonx.string_member "method" value with
      | Some method_name -> (
          match id_member value with
          | None -> `Ok
          | Some id -> (
              match write_stdio server stdio (server_reply method_name id) with
              | Ok () -> `Ok
              | Error (`Timeout _) -> `Malformed "stdio server request response timed out"
              ))
      | None -> (
          match id_member value with
          | None -> `Malformed "JSON-RPC message has neither method nor id"
          | Some id -> (
              match id_as_int id with
              | None -> `Ok
              | Some id ->
                  resolve_pending server id (response_error server value);
                  `Ok)))
  | Jsont.Object _ -> `Malformed "JSON-RPC message has an unsupported jsonrpc version"
  | _ -> `Malformed "JSON-RPC message is not an object"

let parse_and_dispatch_stdio server stdio line =
  match Jsonx.json_of_string line with
  | Error message -> `Malformed (Fmt.str "invalid JSON from stdio server: %s" message)
  | Ok value -> dispatch_stdio server stdio value

let stdio_reader server stdio =
  let rec loop () =
    match Eio.Buf_read.line stdio.reader with
    | "" -> loop ()
    | line -> (
        match parse_and_dispatch_stdio server stdio line with
        | `Ok -> loop ()
        | `Malformed message -> fail_server server message)
    | exception End_of_file ->
        let state = status server in
        if not (status_is_connected state) then
          fail_server server "stdio server closed during startup"
        else if state <> Disabled then
          fail_server server "stdio server closed the connection"
  in
  loop ()

let remove_pending (server : server) id =
  Eio.Mutex.use_rw ~protect:true server.lock (fun () -> Hashtbl.remove server.pending id)

let next_id (server : server) =
  Eio.Mutex.use_rw ~protect:true server.lock (fun () ->
      let id = server.next_id in
      server.next_id <- server.next_id + 1;
      id)

let add_pending (server : server) id pending =
  Eio.Mutex.use_rw ~protect:true server.lock (fun () ->
      if server.closed then false
      else (
        Hashtbl.replace server.pending id pending;
        true))

let await_pending (server : server) id pending =
  let outcome =
    try
      Eio.Time.with_timeout server.clock (timeout_seconds server) (fun () ->
          Ok (Eio.Promise.await pending.promise))
    with Eio.Cancel.Cancelled exception_ ->
      remove_pending server id;
      raise (Eio.Cancel.Cancelled exception_)
  in
  match outcome with
  | Ok value -> value
  | Error `Timeout ->
      remove_pending server id;
      Error (`Timeout server.name)

let rpc_stdio server stdio ~method_name ~params =
  let id = next_id server in
  let promise, resolver = Eio.Promise.create () in
  let pending = { promise; resolver } in
  if not (add_pending server id pending) then Error (`Not_connected server.name)
  else
    let request = request_json id method_name params in
    match
      try write_stdio server stdio request with
      | Eio.Cancel.Cancelled exception_ -> raise (Eio.Cancel.Cancelled exception_)
      | Eio.Io (_, _) as exception_ ->
          remove_pending server id;
          Error (`Transport (server.name, Fmt.str "%a" Eio.Exn.pp exception_))
      | End_of_file ->
          remove_pending server id;
          Error (`Transport (server.name, "stdio server closed while writing"))
      | Unix.Unix_error (error, function_name, argument) ->
          remove_pending server id;
          Error
            (`Transport
               ( server.name,
                 Fmt.str "%s: %s (%s)" function_name (Unix.error_message error) argument
               ))
    with
    | Ok () -> await_pending server id pending
    | Error (`Timeout _) ->
        remove_pending server id;
        Error (`Timeout server.name)
    | Error error -> Error error

type http_read_state = {
  data : Buffer.t;
  mutable data_lines : int;
  mutable answer : (Jsont.json, error) result option;
  mutable replies : Jsont.json list;
  on_request : (Jsont.json -> unit) option;
}

let rpc_response_of_http server expected_id state value =
  match value with
  | Jsont.Object _ when Jsonx.string_member "jsonrpc" value = Some "2.0" -> (
      match Jsonx.string_member "method" value with
      | Some method_name -> (
          match id_member value with
          | None -> Ok ()
          | Some id ->
              let reply = server_reply method_name id in
              (match state.on_request with
              | Some on_request -> on_request reply
              | None -> state.replies <- reply :: state.replies);
              Ok ())
      | None -> (
          match id_member value with
          | None -> Error "JSON-RPC message has neither method nor id"
          | Some id -> (
              match (expected_id, id_as_int id) with
              | Some expected_id, Some id when expected_id = id ->
                  state.answer <- Some (response_error server value);
                  Ok ()
              | _ ->
                  (match id_as_int id with
                  | Some id ->
                      Log.T.debug (fun log ->
                          log "ignoring late or unknown HTTP response from %s (id %d)"
                            server.name id)
                  | None ->
                      Log.T.debug (fun log ->
                          log "ignoring HTTP response with unknown id from %s" server.name));
                  Ok ())))
  | Jsont.Object _ -> Error "JSON-RPC message has an unsupported jsonrpc version"
  | _ -> Error "JSON-RPC message is not an object"

let dispatch_sse_event server expected_id state =
  if state.data_lines = 0 then Ok ()
  else
    let data = Buffer.contents state.data in
    Buffer.clear state.data;
    state.data_lines <- 0;
    match Jsonx.json_of_string data with
    | Error message -> Error (Fmt.str "invalid SSE JSON: %s" message)
    | Ok value -> rpc_response_of_http server expected_id state value

let feed_sse_line server expected_id state line =
  if line = "" then dispatch_sse_event server expected_id state
  else if line.[0] = ':' then Ok ()
  else
    let field, value =
      match String.index_opt line ':' with
      | None -> (line, "")
      | Some index ->
          let value = String.sub line (index + 1) (String.length line - index - 1) in
          let value =
            if String.starts_with ~prefix:" " value then
              String.sub value 1 (String.length value - 1)
            else value
          in
          (String.sub line 0 index, value)
    in
    match field with
    | "event" -> Ok ()
    | "data" ->
        if Buffer.length state.data + String.length value > max_message_bytes then
          Error "SSE event exceeded the message bound"
        else (
          if state.data_lines > 0 then Buffer.add_char state.data '\n';
          Buffer.add_string state.data value;
          state.data_lines <- state.data_lines + 1;
          Ok ())
    | _ -> Ok ()

let read_sse_body server expected_id ~on_request body =
  let state =
    {
      data = Buffer.create 1_024;
      data_lines = 0;
      answer = None;
      replies = [];
      on_request;
    }
  in
  let reader = Eio.Buf_read.of_flow ~max_size:max_message_bytes body in
  let rec loop () =
    match state.answer with
    | Some _ -> Ok state
    | None -> (
        match Eio.Buf_read.line reader with
        | line -> (
            match feed_sse_line server expected_id state line with
            | Error message -> Error (`Transport (server.name, message))
            | Ok () -> loop ())
        | exception End_of_file -> (
            match dispatch_sse_event server expected_id state with
            | Error message -> Error (`Transport (server.name, message))
            | Ok () -> Ok state))
  in
  loop ()

let read_json_body server expected_id ~on_request body =
  let reader = Eio.Buf_read.of_flow ~max_size:max_http_body_bytes body in
  let payload = Eio.Buf_read.take_all reader in
  if String.trim payload = "" then
    Ok { data = Buffer.create 0; data_lines = 0; answer = None; replies = []; on_request }
  else
    match Jsonx.json_of_string payload with
    | Error message ->
        Error (`Transport (server.name, Fmt.str "invalid HTTP JSON: %s" message))
    | Ok value -> (
        let state =
          {
            data = Buffer.create 0;
            data_lines = 0;
            answer = None;
            replies = [];
            on_request;
          }
        in
        match rpc_response_of_http server expected_id state value with
        | Ok () -> Ok state
        | Error message -> Error (`Transport (server.name, message)))

type http_exchange = {
  answer : (Jsont.json, error) result option;
  replies : Jsont.json list;
}

let status_code response = Http.Status.to_int (Http.Response.status response)

let content_type response =
  Option.map String.lowercase_ascii
    (Http.Header.get (Http.Response.headers response) "content-type")

let response_body server expected_id ~on_request response body =
  match content_type response with
  | Some value when String.starts_with ~prefix:"text/event-stream" value ->
      read_sse_body server expected_id ~on_request body
  | _ -> read_json_body server expected_id ~on_request body

let request_headers http =
  Eio.Mutex.use_ro http.session_lock (fun () ->
      let headers = Http.Header.of_list http.headers in
      let headers = Http.Header.replace headers "content-type" "application/json" in
      let headers =
        Http.Header.replace headers "accept" "application/json, text/event-stream"
      in
      let headers =
        Http.Header.replace headers "mcp-protocol-version" http.protocol_version
      in
      match http.session_id with
      | None -> headers
      | Some value -> Http.Header.replace headers "mcp-session-id" value)

let update_session http response =
  match Http.Header.get (Http.Response.headers response) "mcp-session-id" with
  | None -> ()
  | Some value when String.trim value <> "" ->
      Eio.Mutex.use_rw ~protect:true http.session_lock (fun () ->
          http.session_id <- Some value)
  | Some _ -> ()

let http_exchange_raw server http ~expected_id ~on_request value =
  Eio.Switch.run @@ fun sw ->
  let body = Cohttp_eio.Body.of_string (Jsonx.string_of_json value) in
  let response, response_body_flow =
    Cohttp_eio.Client.call http.client ~sw ~headers:(request_headers http) ~body `POST
      http.uri
  in
  update_session http response;
  let status = status_code response in
  if status < 200 || status >= 300 then
    let reader = Eio.Buf_read.of_flow ~max_size:max_http_body_bytes response_body_flow in
    let _ = Eio.Buf_read.take_all reader in
    Error (`Transport (server.name, Fmt.str "HTTP %d" status))
  else
    let* (parsed : http_read_state) =
      response_body server expected_id ~on_request response response_body_flow
    in
    Ok { answer = parsed.answer; replies = parsed.replies }

let http_error_guard server thunk =
  try thunk () with
  | Eio.Cancel.Cancelled exception_ -> raise (Eio.Cancel.Cancelled exception_)
  | Eio.Io (_, _) as exception_ ->
      Error (`Transport (server.name, Fmt.str "%a" Eio.Exn.pp exception_))
  | Eio.Buf_read.Buffer_limit_exceeded ->
      Error (`Transport (server.name, "HTTP response exceeded the 16 MiB bound"))
  | Tls_eio.Tls_alert alert ->
      Error
        (`Transport
           (server.name, Fmt.str "TLS alert: %s" (Tls.Packet.alert_type_to_string alert)))
  | Tls_eio.Tls_failure failure ->
      Error
        (`Transport (server.name, Fmt.str "TLS failure: %a" Tls.Engine.pp_failure failure))
  | End_of_file ->
      Error (`Transport (server.name, "connection closed before the response completed"))
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (`Transport
           ( server.name,
             Fmt.str "%s: %s (%s)" function_name (Unix.error_message error) argument ))
  | Failure message -> Error (`Transport (server.name, message))

let queue_http_reply (http : http) reply =
  if not http.closed then
    Eio.Fiber.first
      (fun () -> Eio.Stream.add http.reply_queue reply)
      (fun () -> Eio.Promise.await http.stopped)

let http_request_callback _server http reply = queue_http_reply http reply

let rec send_http_reply depth (server : server) (http : http) reply =
  if depth > 16 then fail_server server "MCP server request recursion exceeded the bound"
  else if not http.closed then
    match
      Eio.Time.with_timeout server.clock (timeout_seconds server) (fun () ->
          Ok
            (http_error_guard server (fun () ->
                 http_exchange_raw server http ~expected_id:None
                   ~on_request:(Some (send_http_reply (depth + 1) server http))
                   reply)))
    with
    | Ok (Ok exchange) ->
        List.iter (send_http_reply (depth + 1) server http) (List.rev exchange.replies)
    | Ok (Error error) -> fail_server server (error_message error)
    | Error `Timeout -> fail_server server "MCP server request reply timed out"

let http_reply_worker server (http : http) =
  let rec loop () =
    let reply = Eio.Stream.take http.reply_queue in
    if not http.closed then (
      send_http_reply 0 server http reply;
      loop ())
  in
  Eio.Fiber.first loop (fun () -> Eio.Promise.await http.stopped)

let with_http_lock lock fn =
  Eio.Mutex.lock lock;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock lock) fn

let run_http (server : server) http fn =
  match
    Eio.Time.with_timeout server.clock (timeout_seconds server) (fun () ->
        with_http_lock http.writer_lock (fun () -> Ok (http_error_guard server fn)))
  with
  | Ok result -> result
  | Error `Timeout -> Error (`Timeout server.name)

let http_send_message server http message =
  run_http server http (fun () ->
      http_exchange_raw server http ~expected_id:None
        ~on_request:(Some (http_request_callback server http))
        message)

let rec drain_http_replies depth server http replies =
  if depth > 16 then
    Error (`Transport (server.name, "MCP server request recursion exceeded the bound"))
  else
    match replies with
    | [] -> Ok ()
    | reply :: rest ->
        let* exchange = http_send_message server http reply in
        let* () =
          drain_http_replies (depth + 1) server http (List.rev exchange.replies)
        in
        drain_http_replies depth server http rest

let http_rpc server http ~method_name ~params =
  let id = next_id server in
  let request = request_json id method_name params in
  let* exchange =
    run_http server http (fun () ->
        http_exchange_raw server http ~expected_id:(Some id)
          ~on_request:(Some (http_request_callback server http))
          request)
  in
  let* () = drain_http_replies 0 server http (List.rev exchange.replies) in
  Option.value exchange.answer
    ~default:
      (Error (`Transport (server.name, "HTTP response did not contain the request id")))

let http_notify server http ~method_name ~params =
  let request = notification_json method_name params in
  let* exchange =
    run_http server http (fun () ->
        http_exchange_raw server http ~expected_id:None
          ~on_request:(Some (http_request_callback server http))
          request)
  in
  drain_http_replies 0 server http (List.rev exchange.replies)

let wait_ready (server : server) =
  match status server with
  | Connected _ -> Ok ()
  | Failed _ | Disabled -> Error (`Not_connected server.name)
  | Connecting -> (
      match
        Eio.Time.with_timeout server.clock readiness_timeout (fun () ->
            Ok (Eio.Promise.await server.ready))
      with
      | Ok (Ok ()) -> Ok ()
      | Ok (Error _) -> Error (`Not_connected server.name)
      | Error `Timeout -> Error (`Timeout server.name))

let rpc_unchecked server ~method_name ~params =
  match server_transport server with
  | None -> Error (`Not_connected server.name)
  | Some (Stdio stdio) -> rpc_stdio server stdio ~method_name ~params
  | Some (Http_transport http) -> http_rpc server http ~method_name ~params

let notify_unchecked server ~method_name ~params =
  match server_transport server with
  | None -> Error (`Not_connected server.name)
  | Some (Stdio stdio) -> (
      try write_stdio server stdio (notification_json method_name params) with
      | Eio.Cancel.Cancelled exception_ -> raise (Eio.Cancel.Cancelled exception_)
      | Eio.Io (_, _) as exception_ ->
          Error (`Transport (server.name, Fmt.str "%a" Eio.Exn.pp exception_))
      | End_of_file ->
          Error (`Transport (server.name, "stdio server closed while writing")))
  | Some (Http_transport http) -> http_notify server http ~method_name ~params

let rpc server ~method_name ~params =
  let* () = wait_ready server in
  rpc_unchecked server ~method_name ~params

let parse_required_string server field value =
  match Jsonx.string_member field value with
  | Some text -> Ok text
  | None ->
      Error
        (`Transport (server.name, Fmt.str "MCP response is missing string field %s" field))

let parse_tool server value : (tool, error) result =
  let* name = parse_required_string server "name" value in
  let description = Option.value (Jsonx.string_member "description" value) ~default:"" in
  let schema =
    Option.value (Jsonx.member "inputSchema" value) ~default:(json_object [])
  in
  Ok { server = server.name; name; description; schema }

let parse_resource server value : (resource, error) result =
  let* uri = parse_required_string server "uri" value in
  let name = Option.value (Jsonx.string_member "name" value) ~default:uri in
  let mime = Jsonx.string_member "mimeType" value in
  let description = Jsonx.string_member "description" value in
  Ok { server = server.name; uri; name; mime; description }

let parse_prompt_argument server value =
  let* name = parse_required_string server "name" value in
  Ok (name, Option.value (Jsonx.bool_member "required" value) ~default:false)

let parse_prompt server value : (prompt, error) result =
  let* name = parse_required_string server "name" value in
  let description = Option.value (Jsonx.string_member "description" value) ~default:"" in
  let* arguments =
    match array_member "arguments" value with
    | None -> Ok []
    | Some values ->
        let rec loop acc = function
          | [] -> Ok (List.rev acc)
          | value :: rest ->
              let* argument = parse_prompt_argument server value in
              loop (argument :: acc) rest
        in
        loop [] values
  in
  Ok { server = server.name; name; description; arguments }

let next_cursor value =
  match Jsonx.member "nextCursor" value with
  | Some (Jsont.String (cursor, _)) when cursor <> "" -> Some cursor
  | _ -> None

let page_values server field parser value =
  match array_member field value with
  | None ->
      Error
        (`Transport (server.name, Fmt.str "MCP response is missing array field %s" field))
  | Some values ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* parsed = parser server value in
            loop (parsed :: acc) rest
      in
      loop [] values

let list_pages server ~method_name ~field ~parser =
  let rec loop cursor seen page acc =
    if page >= max_pages then
      Error (`Transport (server.name, "MCP pagination exceeded the page bound"))
    else
      let params =
        match cursor with
        | None -> json_object []
        | Some cursor -> json_object [ ("cursor", json_string cursor) ]
      in
      let* value = rpc_unchecked server ~method_name ~params in
      let* values = page_values server field parser value in
      let next = next_cursor value in
      match next with
      | None -> Ok (List.rev (List.rev_append values acc))
      | Some next when List.mem next seen ->
          Error (`Transport (server.name, "MCP pagination repeated a cursor"))
      | Some next ->
          loop (Some next) (next :: seen) (page + 1) (List.rev_append values acc)
  in
  loop None [] 0 []

let initialize (server : server) =
  let params =
    json_object
      [
        ("protocolVersion", json_string protocol_version);
        ("capabilities", json_object []);
        ( "clientInfo",
          json_object
            [
              ("name", json_string "crush");
              ("version", json_string Charamel_cli.Version.current);
            ] );
      ]
  in
  let* value = rpc_unchecked server ~method_name:"initialize" ~params in
  let* selected = parse_required_string server "protocolVersion" value in
  (match server_transport server with
  | Some (Http_transport http) ->
      Eio.Mutex.use_rw ~protect:true http.session_lock (fun () ->
          http.protocol_version <- selected)
  | _ -> ());
  let* () =
    notify_unchecked server ~method_name:"notifications/initialized"
      ~params:(json_object [])
  in
  let* tools =
    list_pages server ~method_name:"tools/list" ~field:"tools" ~parser:parse_tool
  in
  let* resources =
    list_pages server ~method_name:"resources/list" ~field:"resources"
      ~parser:parse_resource
  in
  let* prompts =
    list_pages server ~method_name:"prompts/list" ~field:"prompts" ~parser:parse_prompt
  in
  let* () =
    Eio.Mutex.use_rw ~protect:true server.lock (fun () ->
        if server.closed || server.state <> Connecting then
          Error (`Not_connected server.name)
        else (
          server.tools <- tools;
          server.state <-
            Connected
              {
                tools = List.length tools;
                resources = List.length resources;
                prompts = List.length prompts;
              };
          Ok ()))
  in
  ignore (Eio.Promise.try_resolve server.ready_resolver (Ok ()));
  Ok ()

let setup_http (server : server) (config : Config.mcp) =
  match config.Config.url with
  | None -> Error "HTTP MCP server has no URL"
  | Some url -> (
      let uri = Uri.of_string url in
      match (Uri.scheme uri, Uri.host uri) with
      | Some ("http" | "https"), Some host when host <> "" ->
          let tls_result =
            match Uri.scheme uri with
            | Some "https" -> (
                match Eio_unix.run_in_systhread (fun () -> Ca_certs.authenticator ()) with
                | Error (`Msg message) ->
                    Error (Fmt.str "TLS trust store unavailable: %s" message)
                | Ok authenticator -> (
                    match Tls.Config.client ~authenticator () with
                    | Error (`Msg message) ->
                        Error (Fmt.str "TLS configuration failed: %s" message)
                    | Ok tls_config ->
                        let https endpoint flow =
                          let endpoint_host =
                            Option.value (Uri.host endpoint) ~default:host
                          in
                          match Ipaddr.of_string endpoint_host with
                          | Ok ip -> Tls_eio.client_of_flow tls_config ~ip flow
                          | Error _ ->
                              let domain =
                                Domain_name.of_string_exn endpoint_host
                                |> Domain_name.host_exn
                              in
                              Tls_eio.client_of_flow tls_config ~host:domain flow
                        in
                        Ok (Some https)))
            | _ -> Ok None
          in
          let* https = tls_result in
          let client = Cohttp_eio.Client.make ~https server.net in
          let stopped, stop = Eio.Promise.create () in
          Ok
            (Http_transport
               {
                 client;
                 uri;
                 headers = config.Config.headers;
                 session_id = None;
                 protocol_version;
                 session_lock = Eio.Mutex.create ();
                 writer_lock = Eio.Mutex.create ();
                 reply_queue = Eio.Stream.create 32;
                 stopped;
                 stop;
                 closed = false;
               })
      | Some ("http" | "https"), None -> Error "MCP HTTP URL must include a host"
      | Some scheme, _ -> Error (Fmt.str "unsupported MCP URL scheme %s" scheme)
      | None, _ -> Error "MCP HTTP URL must include http or https")

let merged_environment overrides =
  let values = Hashtbl.create 64 in
  Array.iter
    (fun entry ->
      match String.index_opt entry '=' with
      | None -> Hashtbl.replace values entry ""
      | Some index ->
          Hashtbl.replace values (String.sub entry 0 index)
            (String.sub entry (index + 1) (String.length entry - index - 1)))
    (Unix.environment ());
  List.iter (fun (key, value) -> Hashtbl.replace values key value) overrides;
  Hashtbl.fold (fun key value acc -> (key ^ "=" ^ value) :: acc) values []
  |> Array.of_list

let spawn_stdio (server : server) (config : Config.mcp) =
  match config.Config.command with
  | None -> Error "stdio MCP server has no command"
  | Some command -> (
      try
        let child_input, parent_input = Eio.Process.pipe ~sw:server.sw server.proc_mgr in
        let parent_output, child_output =
          Eio.Process.pipe ~sw:server.sw server.proc_mgr
        in
        let command_line =
          String.concat " " (List.map Filename.quote (command :: config.Config.args))
        in
        let shell = "cd -- " ^ Filename.quote server.cwd ^ " && exec " ^ command_line in
        let process =
          Eio.Process.spawn ~sw:server.sw server.proc_mgr ~stdin:child_input
            ~stdout:child_output ~stderr:Eio.Flow.null
            ~env:(merged_environment config.Config.env)
            [ "/bin/sh"; "-c"; shell ]
        in
        close_flow child_input;
        close_flow child_output;
        Ok
          (Stdio
             {
               process;
               input = parent_input;
               output = parent_output;
               reader = Eio.Buf_read.of_flow ~max_size:max_message_bytes parent_output;
               writer_lock = Eio.Mutex.create ();
             })
      with
      | Eio.Cancel.Cancelled exception_ -> raise (Eio.Cancel.Cancelled exception_)
      | Eio.Io (_, _) as exception_ -> Error (Fmt.str "%a" Eio.Exn.pp exception_)
      | Unix.Unix_error (error, function_name, argument) ->
          Error (Fmt.str "%s: %s (%s)" function_name (Unix.error_message error) argument)
      | Failure message -> Error message)

let run_server (server : server) =
  if connect_server server then
    match server.config.Config.transport with
    | Config.Stdio -> (
        match spawn_stdio server server.config with
        | Error message -> fail_server server message
        | Ok (Stdio stdio as transport) ->
            if install_transport server transport then (
              Eio.Fiber.fork ~sw:server.sw (fun () ->
                  try stdio_reader server stdio with
                  | Eio.Cancel.Cancelled exception_ ->
                      raise (Eio.Cancel.Cancelled exception_)
                  | Eio.Io (_, _) as exception_ ->
                      fail_server server (Fmt.str "%a" Eio.Exn.pp exception_)
                  | Eio.Buf_read.Buffer_limit_exceeded ->
                      fail_server server "stdio message exceeded the 16 MiB bound"
                  | Unix.Unix_error (error, function_name, argument) ->
                      fail_server server
                        (Fmt.str "%s: %s (%s)" function_name (Unix.error_message error)
                           argument)
                  | End_of_file -> fail_server server "stdio server closed the connection");
              match initialize server with
              | Ok () -> ()
              | Error error -> fail_server server (error_message error))
        | Ok (Http_transport _) -> fail_server server "invalid stdio transport setup")
    | Config.Http -> (
        match setup_http server server.config with
        | Error message -> fail_server server message
        | Ok (Http_transport http as transport) ->
            if install_transport server transport then (
              Eio.Fiber.fork ~sw:server.sw (fun () -> http_reply_worker server http);
              match initialize server with
              | Ok () -> ()
              | Error error -> fail_server server (error_message error))
        | Ok (Stdio _) -> fail_server server "invalid HTTP transport setup")

let await_startup clock servers =
  match servers with
  | [] -> ()
  | _ -> (
      let waiters =
        List.map (fun server () -> ignore (Eio.Promise.await server.ready)) servers
      in
      match
        Eio.Time.with_timeout clock readiness_timeout (fun () ->
            Eio.Fiber.all waiters;
            Ok ())
      with
      | Ok _ -> ()
      | Error `Timeout ->
          List.iter
            (fun server ->
              match status server with
              | Connecting -> fail_server server "MCP readiness timed out"
              | Connected _ | Failed _ | Disabled -> ())
            servers)

let create ~sw ~proc_mgr ~net ~clock ~cwd ~(config : Config.t) =
  let servers =
    List.map
      (fun (name, server_config) ->
        let ready, ready_resolver = Eio.Promise.create () in
        {
          name;
          config = server_config;
          sw;
          proc_mgr;
          net;
          clock;
          cwd;
          lock = Eio.Mutex.create ();
          state = Connecting;
          transport = None;
          next_id = 1;
          pending = Hashtbl.create 16;
          ready;
          ready_resolver;
          closed = false;
          tools = [];
        })
      config.Config.mcp
  in
  let client = { lock = Eio.Mutex.create (); closed = false; servers } in
  List.iter (fun server -> Eio.Fiber.fork ~sw (fun () -> run_server server)) servers;
  await_startup clock servers;
  client

let states t = List.map (fun server -> (server.name, status server)) t.servers

let tools t =
  List.concat
    (List.map
       (fun (server : server) ->
         Eio.Mutex.use_ro server.lock (fun () ->
             if status_is_connected server.state then server.tools else []))
       t.servers)

let find_server t name =
  List.find_opt (fun server -> String.equal server.name name) t.servers

let tool_name ~server name = "mcp_" ^ server ^ "_" ^ name

let resources t ~server =
  match find_server t server with
  | None -> Error (`Unknown_server server)
  | Some server ->
      let* () = wait_ready server in
      let* values =
        list_pages server ~method_name:"resources/list" ~field:"resources"
          ~parser:parse_resource
      in
      Eio.Mutex.use_rw ~protect:true server.lock (fun () ->
          (match server.state with
          | Connected counts ->
              server.state <- Connected { counts with resources = List.length values }
          | _ -> ());
          Ok values)

let prompts t ~server =
  match find_server t server with
  | None -> Error (`Unknown_server server)
  | Some server ->
      let* () = wait_ready server in
      let* values =
        list_pages server ~method_name:"prompts/list" ~field:"prompts"
          ~parser:parse_prompt
      in
      Eio.Mutex.use_rw ~protect:true server.lock (fun () ->
          (match server.state with
          | Connected counts ->
              server.state <- Connected { counts with prompts = List.length values }
          | _ -> ());
          Ok values)

let content_of_json server value =
  let* kind = parse_required_string server "type" value in
  match kind with
  | "text" ->
      let* text = parse_required_string server "text" value in
      Ok (Text text)
  | "image" ->
      let* data = parse_required_string server "data" value in
      let mime =
        Option.value
          (Jsonx.string_member "mimeType" value)
          ~default:"application/octet-stream"
      in
      Ok (Image { mime; data })
  | "resource" -> (
      match object_member "resource" value with
      | None ->
          Error (`Transport (server.name, "MCP resource content is missing resource"))
      | Some resource -> (
          let* uri = parse_required_string server "uri" resource in
          let text = Jsonx.string_member "text" resource in
          match Jsonx.string_member "blob" resource with
          | Some data ->
              let mime =
                Option.value
                  (Jsonx.string_member "mimeType" resource)
                  ~default:"application/octet-stream"
              in
              Ok (Image { mime; data })
          | None -> Ok (Resource { uri; text })))
  | other ->
      Error (`Transport (server.name, Fmt.str "unsupported MCP content type %s" other))

let contents_of_result server value =
  match array_member "content" value with
  | None -> Error (`Transport (server.name, "MCP result is missing content"))
  | Some values ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* parsed = content_of_json server value in
            loop (parsed :: acc) rest
      in
      loop [] values

let call t ~server ~tool ~input =
  match find_server t server with
  | None -> Error (`Unknown_server server)
  | Some server ->
      let params = json_object [ ("name", json_string tool); ("arguments", input) ] in
      let* result = rpc server ~method_name:"tools/call" ~params in
      let* content = contents_of_result server result in
      Ok (content, Option.value (Jsonx.bool_member "isError" result) ~default:false)

let read_resource t ~server ~uri =
  match find_server t server with
  | None -> Error (`Unknown_server server)
  | Some server -> (
      let params = json_object [ ("uri", json_string uri) ] in
      let* result = rpc server ~method_name:"resources/read" ~params in
      match array_member "contents" result with
      | None ->
          Error
            (`Transport (server.name, "MCP resources/read result is missing contents"))
      | Some values ->
          let rec loop acc = function
            | [] -> Ok (List.rev acc)
            | value :: rest ->
                let uri = Jsonx.string_member "uri" value in
                let mime =
                  Option.value
                    (Jsonx.string_member "mimeType" value)
                    ~default:"application/octet-stream"
                in
                let* parsed =
                  match
                    ( Jsonx.string_member "blob" value,
                      Jsonx.string_member "text" value,
                      uri )
                  with
                  | Some data, _, _ -> Ok (Image { mime; data })
                  | None, Some text, Some uri -> Ok (Resource { uri; text = Some text })
                  | None, None, Some uri -> Ok (Resource { uri; text = None })
                  | _ ->
                      Error
                        (`Transport (server.name, "MCP resource content is malformed"))
                in
                loop (parsed :: acc) rest
          in
          loop [] values)

let content_text contents =
  let render = function
    | Text text -> text
    | Image { mime; data } -> Fmt.str "[image %s, %d bytes]" mime (String.length data)
    | Resource { uri; text = None } -> Fmt.str "[resource %s]" uri
    | Resource { uri; text = Some text } -> Fmt.str "[resource %s]\n%s" uri text
  in
  String.concat "\n" (List.map render contents)

let prompt_message_text server value =
  match Jsonx.member "content" value with
  | Some content -> (
      match content with
      | Jsont.Array (values, _) ->
          let rec loop acc = function
            | [] -> Ok (content_text (List.rev acc))
            | value :: rest ->
                let* parsed = content_of_json server value in
                loop (parsed :: acc) rest
          in
          loop [] values
      | _ ->
          let* parsed = content_of_json server content in
          Ok (content_text [ parsed ]))
  | None -> Error (`Transport (server.name, "MCP prompt message is missing content"))

let get_prompt t ~server ~name ~args =
  match find_server t server with
  | None -> Error (`Unknown_server server)
  | Some server -> (
      let arguments =
        args
        |> List.map (fun (key, value) ->
            Jsont.Json.mem (Jsont.Json.name key) (json_string value))
        |> Jsont.Json.object'
      in
      let params = json_object [ ("name", json_string name); ("arguments", arguments) ] in
      let* result = rpc server ~method_name:"prompts/get" ~params in
      match array_member "messages" result with
      | None ->
          Error (`Transport (server.name, "MCP prompts/get result is missing messages"))
      | Some values ->
          let rec loop acc = function
            | [] -> Ok (String.concat "\n\n" (List.rev acc))
            | value :: rest ->
                let* text = prompt_message_text server value in
                loop (text :: acc) rest
          in
          loop [] values)

let close t =
  let should_close =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        if t.closed then false
        else (
          t.closed <- true;
          true))
  in
  if should_close then List.iter close_server t.servers
