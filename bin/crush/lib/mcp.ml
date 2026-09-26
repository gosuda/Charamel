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

let max_stdio_line_bytes = 16 * 1024 * 1024
let max_sse_line_bytes = 16 * 1024 * 1024
let max_sse_event_bytes = 16 * 1024 * 1024
let max_http_body_bytes = 16 * 1024 * 1024
let max_pages = 1_024
let reply_queue_bound = 32
let reply_recursion_bound = 16
let readiness_timeout = 10.
let protocol_version = "2025-06-18"

open Lwt.Infix
open Lwt_result.Syntax

let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.int value
let json_list values = Jsont.Json.list values

let json_object members =
  members
  |> List.map (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
  |> Jsont.Json.object'

let error_message = function
  | `Unknown_server server -> Fmt.str "unknown MCP server %s" server
  | `Not_connected server -> Fmt.str "MCP server %s is not connected" server
  | `Rpc (_, code, message) -> Fmt.str "JSON-RPC %d: %s" code message
  | `Timeout _server -> Fmt.str "request timed out after %g seconds" readiness_timeout
  | `Transport (_, message) -> message

type pending = {
  promise : (Jsont.json, error) result Lwt.t;
  resolver : (Jsont.json, error) result Lwt.u;
}

type stdio = {
  process : Charamel_os.Process.t;
  input : Lwt_io.output_channel;
  output : Lwt_io.input_channel;
  writer_lock : Lwt_mutex.t;
}

type http = {
  uri : Uri.t;
  headers : (string * string) list;
  mutable session_id : string option;
  mutable protocol_version : string;
  writer_lock : Lwt_mutex.t;
  mutable replies : Jsont.json list;
  reply_wake : unit Lwt_condition.t;
  mutable closed : bool;
}

type transport = Stdio of stdio | Http_transport of http

type server = {
  name : string;
  config : Config.mcp;
  cwd : string;
  mutable state : state;
  mutable transport : transport option;
  mutable next_id : int;
  pending : (int, pending) Hashtbl.t;
  ready : (unit, string) result Lwt.t;
  ready_resolver : (unit, string) result Lwt.u;
  mutable closed : bool;
  mutable tools : tool list;
}

type t = {
  mutable closed : bool;
  mutable fibers : unit Lwt.t list;
  servers : server list;
}

let status_is_connected = function Connected _ -> true | _ -> false
let status (server : server) = server.state
let server_transport (server : server) = if server.closed then None else server.transport

let settle promise resolver value =
  match Lwt.state promise with
  | Lwt.Sleep ->
      Lwt.wakeup_later resolver value;
      true
  | _ -> false

let settle_pending pending value = settle pending.promise pending.resolver value

let guarded f =
  Lwt.catch f (fun exn ->
      match exn with
      | Lwt.Canceled -> Lwt.return_unit
      | _ ->
          Log.T.err (fun log -> log "MCP fiber failed: %s" (Printexc.to_string exn));
          Lwt.return_unit)

let fork f = ignore (guarded f)

let close_channel channel =
  Lwt.catch (fun () -> Lwt_io.abort channel) (fun _ -> Lwt.return_unit)

let stop_http (http : http) =
  if not http.closed then (
    http.closed <- true;
    Lwt_condition.broadcast http.reply_wake ())

let cleanup_transport = function
  | Stdio stdio ->
      close_channel stdio.input >>= fun () ->
      (* A wedged child must not fail the close: [stop] asks the group to leave, waits the
         grace window, forces the remainder away, and returns without collecting the exit
         status — the reaper started at [spawn] collects it. *)
      close_channel stdio.output >>= fun () -> Charamel_os.Process.stop stdio.process
  | Http_transport http ->
      stop_http http;
      Lwt.return_unit

let reject_pending (server : server) error =
  let values = Hashtbl.fold (fun _ value acc -> value :: acc) server.pending [] in
  Hashtbl.clear server.pending;
  List.iter (fun item -> ignore (settle_pending item (Error error))) values

let fail_server (server : server) message =
  let transport =
    if server.closed || server.state = Disabled then None
    else (
      server.closed <- true;
      server.state <- Failed message;
      ignore (settle server.ready server.ready_resolver (Error message));
      let transport = server.transport in
      server.transport <- None;
      transport)
  in
  reject_pending server (`Transport (server.name, message));
  Option.iter (fun transport -> fork (fun () -> cleanup_transport transport)) transport

let connect_server (server : server) =
  if server.closed || server.state <> Connecting then false
  else (
    server.state <- Connecting;
    true)

let install_transport (server : server) transport =
  let accepted =
    if server.closed then false
    else (
      server.transport <- Some transport;
      true)
  in
  if not accepted then fork (fun () -> cleanup_transport transport);
  accepted

let close_server (server : server) =
  let transport =
    if server.closed then (
      server.state <- Disabled;
      None)
    else (
      server.closed <- true;
      server.state <- Disabled;
      ignore (settle server.ready server.ready_resolver (Error "client closed"));
      let values = Hashtbl.fold (fun _ value acc -> value :: acc) server.pending [] in
      Hashtbl.clear server.pending;
      List.iter
        (fun item ->
          ignore (settle_pending item (Error (`Transport (server.name, "client closed")))))
        values;
      let transport = server.transport in
      server.transport <- None;
      transport)
  in
  match transport with
  | None -> Lwt.return_unit
  | Some transport -> cleanup_transport transport

let response_error server value =
  match Jsonx.object_member "error" value with
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
  let attempt () =
    Lwt_mutex.with_lock stdio.writer_lock (fun () ->
        Lwt_io.write stdio.input line >>= fun () -> Lwt_io.flush stdio.input)
  in
  Lwt.catch
    (fun () ->
      Lwt.map Result.ok
        (Lwt_unix.with_timeout (timeout_seconds server) (fun () -> attempt ())))
    (function
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | Lwt_unix.Timeout -> Lwt.return (Error (`Timeout server.name))
      | End_of_file ->
          Lwt.return
            (Error (`Transport (server.name, "stdio server closed while writing")))
      | Unix.Unix_error (error, function_name, argument) ->
          Lwt.return
            (Error
               (`Transport
                  ( server.name,
                    Fmt.str "%s: %s (%s)" function_name (Unix.error_message error)
                      argument )))
      | exn -> Lwt.return (Error (`Transport (server.name, Printexc.to_string exn))))

let resolve_pending (server : server) id result =
  match Hashtbl.find_opt server.pending id with
  | None ->
      Log.T.debug (fun log ->
          log "ignoring late or unknown response from %s (id %d)" server.name id)
  | Some pending ->
      Hashtbl.remove server.pending id;
      ignore (settle_pending pending result)

let dispatch_stdio server stdio value =
  match value with
  | Jsont.Object _ when Jsonx.string_member "jsonrpc" value = Some "2.0" -> (
      match Jsonx.string_member "method" value with
      | Some method_name -> (
          match id_member value with
          | None -> Lwt.return `Ok
          | Some id -> (
              write_stdio server stdio (server_reply method_name id) >|= function
              | Ok () -> `Ok
              | Error (`Timeout _) -> `Malformed "stdio server request response timed out"
              | Error (`Transport _) ->
                  `Malformed "stdio server request response could not be written"))
      | None -> (
          match id_member value with
          | None -> Lwt.return (`Malformed "JSON-RPC message has neither method nor id")
          | Some id -> (
              match id_as_int id with
              | None -> Lwt.return `Ok
              | Some id ->
                  resolve_pending server id (response_error server value);
                  Lwt.return `Ok)))
  | Jsont.Object _ ->
      Lwt.return (`Malformed "JSON-RPC message has an unsupported jsonrpc version")
  | _ -> Lwt.return (`Malformed "JSON-RPC message is not an object")

let parse_and_dispatch_stdio server stdio line =
  match Jsonx.json_of_string line with
  | Error message ->
      Lwt.return (`Malformed (Fmt.str "invalid JSON from stdio server: %s" message))
  | Ok value -> dispatch_stdio server stdio value

let strip_trailing_cr line =
  if String.ends_with ~suffix:"\r" line then String.sub line 0 (String.length line - 1)
  else line

type line_outcome = Line of string | Eof | Too_long

let read_stdio_line ic =
  let buffer = Buffer.create 1_024 in
  let rec loop read_any =
    if Buffer.length buffer > max_stdio_line_bytes then Lwt.return Too_long
    else
      Lwt_io.read_char_opt ic >>= function
      | None ->
          if read_any then Lwt.return (Line (strip_trailing_cr (Buffer.contents buffer)))
          else Lwt.return Eof
      | Some '\n' -> Lwt.return (Line (strip_trailing_cr (Buffer.contents buffer)))
      | Some character ->
          Buffer.add_char buffer character;
          loop true
  in
  loop false

let report_closed_stdio server =
  let state = status server in
  if not (status_is_connected state) then
    fail_server server "stdio server closed during startup"
  else if state <> Disabled then fail_server server "stdio server closed the connection"

let stdio_reader server stdio =
  let rec loop () =
    read_stdio_line stdio.output >>= function
    | Eof ->
        report_closed_stdio server;
        Lwt.return_unit
    | Too_long ->
        fail_server server "stdio message exceeded the 16 MiB bound";
        Lwt.return_unit
    | Line "" -> loop ()
    | Line line -> (
        parse_and_dispatch_stdio server stdio line >>= function
        | `Ok -> loop ()
        | `Malformed message ->
            fail_server server message;
            Lwt.return_unit)
  in
  Lwt.catch loop (fun exn ->
      match exn with
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | End_of_file ->
          report_closed_stdio server;
          Lwt.return_unit
      | Unix.Unix_error (error, function_name, argument) ->
          fail_server server
            (Fmt.str "%s: %s (%s)" function_name (Unix.error_message error) argument);
          Lwt.return_unit
      | Sys_error message | Failure message | Invalid_argument message ->
          fail_server server message;
          Lwt.return_unit
      | exn ->
          fail_server server (Printexc.to_string exn);
          Lwt.return_unit)

let remove_pending (server : server) id = Hashtbl.remove server.pending id

let next_id (server : server) =
  let id = server.next_id in
  server.next_id <- server.next_id + 1;
  id

let add_pending (server : server) id pending =
  if server.closed then false
  else (
    Hashtbl.replace server.pending id pending;
    true)

let await_pending server id pending =
  let timeout =
    Lwt_unix.sleep (timeout_seconds server) >|= fun () -> Error (`Timeout server.name)
  in
  let raced = Lwt.pick [ pending.promise; timeout ] in
  Lwt.on_cancel raced (fun () -> remove_pending server id);
  raced >|= fun result ->
  remove_pending server id;
  result

let rpc_stdio server stdio ~method_name ~params =
  let id = next_id server in
  let promise, resolver = Lwt.task () in
  let pending = { promise; resolver } in
  if not (add_pending server id pending) then
    Lwt.return_error (`Not_connected server.name)
  else
    let request = request_json id method_name params in
    write_stdio server stdio request >>= function
    | Ok () -> await_pending server id pending
    | Error error ->
        remove_pending server id;
        Lwt.return_error error

type http_read_state = {
  data : Buffer.t;
  mutable data_lines : int;
  mutable answer : (Jsont.json, error) result option;
  mutable replies : Jsont.json list;
  on_request : (Jsont.json -> unit Lwt.t) option;
}

let empty_state on_request =
  { data = Buffer.create 0; data_lines = 0; answer = None; replies = []; on_request }

let rpc_response_of_http server expected_id state value =
  match value with
  | Jsont.Object _ when Jsonx.string_member "jsonrpc" value = Some "2.0" -> (
      match Jsonx.string_member "method" value with
      | Some method_name -> (
          match id_member value with
          | None -> Lwt.return_ok ()
          | Some id ->
              let reply = server_reply method_name id in
              (match state.on_request with
                | Some on_request -> on_request reply
                | None ->
                    state.replies <- reply :: state.replies;
                    Lwt.return_unit)
              >|= fun () -> Ok ())
      | None -> (
          match id_member value with
          | None -> Lwt.return_error "JSON-RPC message has neither method nor id"
          | Some id -> (
              match (expected_id, id_as_int id) with
              | Some expected_id, Some id when expected_id = id ->
                  state.answer <- Some (response_error server value);
                  Lwt.return_ok ()
              | _ ->
                  (match id_as_int id with
                  | Some id ->
                      Log.T.debug (fun log ->
                          log "ignoring late or unknown HTTP response from %s (id %d)"
                            server.name id)
                  | None ->
                      Log.T.debug (fun log ->
                          log "ignoring HTTP response with unknown id from %s" server.name));
                  Lwt.return_ok ())))
  | Jsont.Object _ ->
      Lwt.return_error "JSON-RPC message has an unsupported jsonrpc version"
  | _ -> Lwt.return_error "JSON-RPC message is not an object"

let dispatch_sse_event server expected_id state =
  if state.data_lines = 0 then Lwt.return_ok ()
  else
    let data = Buffer.contents state.data in
    Buffer.clear state.data;
    state.data_lines <- 0;
    match Jsonx.json_of_string data with
    | Error message -> Lwt.return_error (Fmt.str "invalid SSE JSON: %s" message)
    | Ok value -> rpc_response_of_http server expected_id state value

let feed_sse_line server expected_id state line =
  if line = "" then dispatch_sse_event server expected_id state
  else if line.[0] = ':' then Lwt.return_ok ()
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
    | "event" -> Lwt.return_ok ()
    | "data" ->
        if Buffer.length state.data + String.length value > max_sse_event_bytes then
          Lwt.return_error "SSE event exceeded the message bound"
        else (
          if state.data_lines > 0 then Buffer.add_char state.data '\n';
          Buffer.add_string state.data value;
          state.data_lines <- state.data_lines + 1;
          Lwt.return_ok ())
    | _ -> Lwt.return_ok ()

let read_sse_body server (http : http) expected_id ~on_request ic =
  let state =
    {
      data = Buffer.create 1_024;
      data_lines = 0;
      answer = None;
      replies = [];
      on_request;
    }
  in
  let fail message = Lwt.return (Error (`Transport (server.name, message))) in
  let finish () =
    dispatch_sse_event server expected_id state >|= function
    | Error message -> Error (`Transport (server.name, message))
    | Ok () -> Ok state
  in
  let rec loop () =
    match state.answer with
    | Some _ -> Lwt.return (Ok state)
    | None -> if http.closed then fail "MCP client closed the transport" else read_step ()
  and read_step () =
    Charamel_net.read_line ~bound:max_sse_line_bytes ~timeout:(timeout_seconds server) ic
    >>= function
    | Error net_error -> fail (Charamel_net.error_message net_error)
    | Ok None -> finish ()
    | Ok (Some line) -> (
        feed_sse_line server expected_id state line >>= function
        | Error message -> fail message
        | Ok () -> loop ())
  in
  loop ()

let read_all ic =
  let buffer = Buffer.create 4_096 in
  let rec loop () =
    if Buffer.length buffer > max_http_body_bytes then
      Lwt.return_error
        (Fmt.str "HTTP response exceeded the %d byte bound" max_http_body_bytes)
    else
      Lwt_io.read ~count:8_192 ic >>= function
      | "" -> Lwt.return_ok (Buffer.contents buffer)
      | chunk ->
          Buffer.add_string buffer chunk;
          loop ()
  in
  loop ()

let read_json_body server expected_id ~on_request ic =
  read_all ic >>= function
  | Error message -> Lwt.return_error (`Transport (server.name, message))
  | Ok payload -> (
      if String.trim payload = "" then Lwt.return_ok (empty_state on_request)
      else
        match Jsonx.json_of_string payload with
        | Error message ->
            Lwt.return_error
              (`Transport (server.name, Fmt.str "invalid HTTP JSON: %s" message))
        | Ok value -> (
            let state = empty_state on_request in
            rpc_response_of_http server expected_id state value >|= function
            | Ok () -> Ok state
            | Error message -> Error (`Transport (server.name, message))))

type http_exchange = {
  answer : (Jsont.json, error) result option;
  replies : Jsont.json list;
}

let content_type response =
  Option.map String.lowercase_ascii
    (Http.Header.get (Http.Response.headers response) "content-type")

let response_body server (http : http) expected_id ~on_request response ic =
  match content_type response with
  | Some value when String.starts_with ~prefix:"text/event-stream" value ->
      read_sse_body server http expected_id ~on_request ic
  | _ -> read_json_body server expected_id ~on_request ic

let reserved_headers =
  [ "content-type"; "accept"; "mcp-protocol-version"; "mcp-session-id" ]

let request_headers http =
  let reserved (name, _) = List.mem (String.lowercase_ascii name) reserved_headers in
  let headers =
    ("mcp-protocol-version", http.protocol_version)
    :: ("content-type", "application/json")
    :: ("accept", "application/json, text/event-stream")
    :: List.filter (fun header -> not (reserved header)) http.headers
  in
  match http.session_id with
  | None -> headers
  | Some value -> ("mcp-session-id", value) :: headers

let update_session http response =
  match Http.Header.get (Http.Response.headers response) "mcp-session-id" with
  | Some value when String.trim value <> "" -> http.session_id <- Some value
  | _ -> ()

let http_exchange_raw server http ~expected_id ~on_request value =
  Charamel_net.call_raw ~timeout:(timeout_seconds server) ~headers:(request_headers http)
    ~meth:`POST
    ~body:(Some (Jsonx.string_of_json value))
    http.uri
  >>= function
  | Error (`Http ({ status; _ } : Charamel_net.http_error)) ->
      Lwt.return_error (`Transport (server.name, Fmt.str "HTTP %d" status))
  | Error net_error ->
      Lwt.return_error (`Transport (server.name, Charamel_net.error_message net_error))
  | Ok (response, ic) ->
      update_session http response;
      let read () =
        response_body server http expected_id ~on_request response ic >|= function
        | Ok (parsed : http_read_state) ->
            Ok { answer = parsed.answer; replies = parsed.replies }
        | Error error -> Error error
      in
      Lwt.finalize read (fun () -> close_channel ic)

let http_error_guard server thunk =
  Lwt.catch thunk (fun exn ->
      match exn with
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | End_of_file ->
          Lwt.return
            (Error
               (`Transport (server.name, "connection closed before the response completed")))
      | Unix.Unix_error (error, function_name, argument) ->
          Lwt.return
            (Error
               (`Transport
                  ( server.name,
                    Fmt.str "%s: %s (%s)" function_name (Unix.error_message error)
                      argument )))
      | Sys_error message | Failure message | Invalid_argument message ->
          Lwt.return (Error (`Transport (server.name, message)))
      | exn -> Lwt.return (Error (`Transport (server.name, Printexc.to_string exn))))

let rec queue_http_reply (http : http) reply =
  if http.closed then Lwt.return_unit
  else if List.length http.replies >= reply_queue_bound then
    Lwt_condition.wait http.reply_wake >>= fun () -> queue_http_reply http reply
  else (
    http.replies <- http.replies @ [ reply ];
    Lwt_condition.broadcast http.reply_wake ();
    Lwt.return_unit)

let take_http_reply (http : http) =
  let rec loop () =
    match http.replies with
    | reply :: rest ->
        http.replies <- rest;
        Lwt_condition.broadcast http.reply_wake ();
        Lwt.return (Some reply)
    | [] when http.closed -> Lwt.return_none
    | [] -> Lwt_condition.wait http.reply_wake >>= loop
  in
  loop ()

let http_request_callback _server http reply = queue_http_reply http reply

let rec send_http_reply depth (server : server) (http : http) reply =
  if depth > reply_recursion_bound then
    Lwt.return (fail_server server "MCP server request recursion exceeded the bound")
  else if http.closed then Lwt.return_unit
  else
    let attempt () =
      http_error_guard server (fun () ->
          http_exchange_raw server http ~expected_id:None
            ~on_request:(Some (send_http_reply (depth + 1) server http))
            reply)
    in
    let timed =
      Lwt.catch
        (fun () -> Lwt_unix.with_timeout (timeout_seconds server) attempt)
        (function
          | Lwt_unix.Timeout ->
              Lwt.return
                (Error (`Transport (server.name, "MCP server request reply timed out")))
          | exn -> Lwt.fail exn)
    in
    timed >>= function
    | Ok exchange ->
        Lwt_list.iter_s
          (send_http_reply (depth + 1) server http)
          (List.rev exchange.replies)
    | Error error -> Lwt.return (fail_server server (error_message error))

let rec http_reply_worker server (http : http) =
  take_http_reply http >>= function
  | None -> Lwt.return_unit
  | Some reply ->
      if http.closed then Lwt.return_unit
      else send_http_reply 0 server http reply >>= fun () -> http_reply_worker server http

let run_http (server : server) http fn =
  Lwt.catch
    (fun () ->
      Lwt_unix.with_timeout (timeout_seconds server) (fun () ->
          Lwt_mutex.with_lock http.writer_lock (fun () -> http_error_guard server fn)))
    (function
      | Lwt_unix.Timeout -> Lwt.return (Error (`Timeout server.name))
      | exn -> Lwt.fail exn)

let http_send_message server http message =
  run_http server http (fun () ->
      http_exchange_raw server http ~expected_id:None
        ~on_request:(Some (http_request_callback server http))
        message)

let rec drain_http_replies depth server http replies =
  if depth > reply_recursion_bound then
    Lwt.return
      (Error (`Transport (server.name, "MCP server request recursion exceeded the bound")))
  else
    match replies with
    | [] -> Lwt.return_ok ()
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
  Lwt.return
    (Option.value exchange.answer
       ~default:
         (Error (`Transport (server.name, "HTTP response did not contain the request id"))))

let http_notify server http ~method_name ~params =
  let request = notification_json method_name params in
  let* exchange =
    run_http server http (fun () ->
        http_exchange_raw server http ~expected_id:None
          ~on_request:(Some (http_request_callback server http))
          request)
  in
  drain_http_replies 0 server http (List.rev exchange.replies)

let await_ready (server : server) =
  Lwt.choose
    [
      ( server.ready >|= function
        | Ok () -> Ok ()
        | Error _ -> Error (`Not_connected server.name) );
      (Lwt_unix.sleep readiness_timeout >|= fun () -> Error (`Timeout server.name));
    ]

let wait_ready (server : server) =
  match status server with
  | Connected _ -> Lwt.return_ok ()
  | Failed _ | Disabled -> Lwt.return_error (`Not_connected server.name)
  | Connecting -> await_ready server

let rpc_unchecked server ~method_name ~params =
  match server_transport server with
  | None -> Lwt.return_error (`Not_connected server.name)
  | Some (Stdio stdio) -> rpc_stdio server stdio ~method_name ~params
  | Some (Http_transport http) -> http_rpc server http ~method_name ~params

let notify_unchecked server ~method_name ~params =
  match server_transport server with
  | None -> Lwt.return_error (`Not_connected server.name)
  | Some (Stdio stdio) -> write_stdio server stdio (notification_json method_name params)
  | Some (Http_transport http) -> http_notify server http ~method_name ~params

let rpc server ~method_name ~params =
  let* () = wait_ready server in
  rpc_unchecked server ~method_name ~params

let parse_required_string server field value =
  match Jsonx.string_member field value with
  | Some text -> Lwt.return_ok text
  | None ->
      Lwt.return_error
        (`Transport (server.name, Fmt.str "MCP response is missing string field %s" field))

let parse_tool server value : (tool, error) result Lwt.t =
  let* name = parse_required_string server "name" value in
  let description = Option.value (Jsonx.string_member "description" value) ~default:"" in
  let schema =
    Option.value (Jsonx.member "inputSchema" value) ~default:(json_object [])
  in
  Lwt.return_ok { server = server.name; name; description; schema }

let parse_resource server value : (resource, error) result Lwt.t =
  let* uri = parse_required_string server "uri" value in
  let name = Option.value (Jsonx.string_member "name" value) ~default:uri in
  let mime = Jsonx.string_member "mimeType" value in
  let description = Jsonx.string_member "description" value in
  Lwt.return_ok { server = server.name; uri; name; mime; description }

let parse_prompt_argument server value =
  let* name = parse_required_string server "name" value in
  Lwt.return_ok (name, Option.value (Jsonx.bool_member "required" value) ~default:false)

let parse_prompt server value : (prompt, error) result Lwt.t =
  let* name = parse_required_string server "name" value in
  let description = Option.value (Jsonx.string_member "description" value) ~default:"" in
  let* arguments =
    match Jsonx.array_member "arguments" value with
    | None -> Lwt.return_ok []
    | Some values ->
        let rec loop acc = function
          | [] -> Lwt.return_ok (List.rev acc)
          | value :: rest ->
              let* argument = parse_prompt_argument server value in
              loop (argument :: acc) rest
        in
        loop [] values
  in
  Lwt.return_ok { server = server.name; name; description; arguments }

let next_cursor value =
  match Jsonx.member "nextCursor" value with
  | Some (Jsont.String (cursor, _)) when cursor <> "" -> Some cursor
  | _ -> None

let page_values server field parser value =
  match Jsonx.array_member field value with
  | None ->
      Lwt.return_error
        (`Transport (server.name, Fmt.str "MCP response is missing array field %s" field))
  | Some values ->
      let rec loop acc = function
        | [] -> Lwt.return_ok (List.rev acc)
        | value :: rest ->
            let* parsed = parser server value in
            loop (parsed :: acc) rest
      in
      loop [] values

let list_pages server ~method_name ~field ~parser =
  let rec loop cursor seen page acc =
    if page >= max_pages then
      Lwt.return_error
        (`Transport (server.name, "MCP pagination exceeded the page bound"))
    else
      let params =
        match cursor with
        | None -> json_object []
        | Some cursor -> json_object [ ("cursor", json_string cursor) ]
      in
      let* value = rpc_unchecked server ~method_name ~params in
      let* values = page_values server field parser value in
      match next_cursor value with
      | None -> Lwt.return_ok (List.rev (List.rev_append values acc))
      | Some next when List.mem next seen ->
          Lwt.return_error (`Transport (server.name, "MCP pagination repeated a cursor"))
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
  | Some (Http_transport http) -> http.protocol_version <- selected
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
  if server.closed || server.state <> Connecting then
    Lwt.return_error (`Not_connected server.name)
  else (
    server.tools <- tools;
    server.state <-
      Connected
        {
          tools = List.length tools;
          resources = List.length resources;
          prompts = List.length prompts;
        };
    ignore (settle server.ready server.ready_resolver (Ok ()));
    Lwt.return_ok ())

let setup_http (config : Config.mcp) =
  match config.Config.url with
  | None -> Error "HTTP MCP server has no URL"
  | Some url -> (
      let uri = Uri.of_string url in
      match (Uri.scheme uri, Uri.host uri) with
      | Some ("http" | "https"), Some host when host <> "" ->
          Ok
            (Http_transport
               {
                 uri;
                 headers = config.Config.headers;
                 session_id = None;
                 protocol_version;
                 writer_lock = Lwt_mutex.create ();
                 replies = [];
                 reply_wake = Lwt_condition.create ();
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

let exec_prefix = if Sys.win32 then "" else "exec "

let spawn_stdio (server : server) (config : Config.mcp) =
  match config.Config.command with
  | None -> Error "stdio MCP server has no command"
  | Some command -> (
      let command_line =
        String.concat " "
          (List.map Charamel_os.Shell.quote (command :: config.Config.args))
      in
      let argv = Charamel_os.Shell.command ~cwd:server.cwd (exec_prefix ^ command_line) in
      try
        let process =
          Charamel_os.Process.spawn
            ~env:(merged_environment config.Config.env)
            ~stdin:`Pipe ~stdout:`Pipe ~stderr:`Null argv
        in
        Ok
          (Stdio
             {
               process;
               input = Charamel_os.Process.stdin_w process;
               output = Charamel_os.Process.stdout_r process;
               writer_lock = Lwt_mutex.create ();
             })
      with
      | Unix.Unix_error (error, function_name, argument) ->
          Error (Fmt.str "%s: %s (%s)" function_name (Unix.error_message error) argument)
      | Failure message -> Error message)

let start_stdio_server server =
  match spawn_stdio server server.config with
  | Error message -> Lwt.return (fail_server server message)
  | Ok (Stdio stdio as transport) ->
      if not (install_transport server transport) then Lwt.return_unit
      else (
        fork (fun () -> stdio_reader server stdio);
        initialize server >|= function
        | Ok () -> ()
        | Error error -> fail_server server (error_message error))
  | Ok (Http_transport _) ->
      Lwt.return (fail_server server "invalid stdio transport setup")

let start_http_server server =
  match setup_http server.config with
  | Error message -> Lwt.return (fail_server server message)
  | Ok (Http_transport http as transport) ->
      if not (install_transport server transport) then Lwt.return_unit
      else (
        fork (fun () -> http_reply_worker server http);
        initialize server >|= function
        | Ok () -> ()
        | Error error -> fail_server server (error_message error))
  | Ok (Stdio _) -> Lwt.return (fail_server server "invalid HTTP transport setup")

let run_server (server : server) =
  if not (connect_server server) then Lwt.return_unit
  else
    match server.config.Config.transport with
    | Config.Stdio -> start_stdio_server server
    | Config.Http -> start_http_server server

let await_startup servers =
  let settled = Lwt.join (List.map (fun server -> Lwt.map ignore server.ready) servers) in
  Lwt.choose
    [
      (settled >|= fun () -> `Complete);
      (Lwt_unix.sleep readiness_timeout >|= fun () -> `Timed_out);
    ]
  >>= function
  | `Complete -> Lwt.return_unit
  | `Timed_out ->
      List.iter
        (fun server ->
          match status server with
          | Connecting -> fail_server server "MCP readiness timed out"
          | Connected _ | Failed _ | Disabled -> ())
        servers;
      Lwt.return_unit

let create ~cwd ~(config : Config.t) =
  let servers =
    List.map
      (fun (name, server_config) ->
        let ready, ready_resolver = Lwt.task () in
        {
          name;
          config = server_config;
          cwd;
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
  let client = { closed = false; fibers = []; servers } in
  client.fibers <- List.map (fun server -> guarded (fun () -> run_server server)) servers;
  await_startup servers >|= fun () -> client

let states t = List.map (fun server -> (server.name, status server)) t.servers

let tools t =
  List.concat
    (List.map
       (fun (server : server) ->
         if status_is_connected server.state then server.tools else [])
       t.servers)

let find_server t name =
  List.find_opt (fun server -> String.equal server.name name) t.servers

let tool_name ~server name = "mcp_" ^ server ^ "_" ^ name

let resources t ~server =
  match find_server t server with
  | None -> Lwt.return_error (`Unknown_server server)
  | Some server ->
      let* () = wait_ready server in
      let* values =
        list_pages server ~method_name:"resources/list" ~field:"resources"
          ~parser:parse_resource
      in
      (match server.state with
      | Connected counts ->
          server.state <- Connected { counts with resources = List.length values }
      | _ -> ());
      Lwt.return_ok values

let prompts t ~server =
  match find_server t server with
  | None -> Lwt.return_error (`Unknown_server server)
  | Some server ->
      let* () = wait_ready server in
      let* values =
        list_pages server ~method_name:"prompts/list" ~field:"prompts"
          ~parser:parse_prompt
      in
      (match server.state with
      | Connected counts ->
          server.state <- Connected { counts with prompts = List.length values }
      | _ -> ());
      Lwt.return_ok values

let content_of_json server value =
  let* kind = parse_required_string server "type" value in
  match kind with
  | "text" ->
      let* text = parse_required_string server "text" value in
      Lwt.return_ok (Text text)
  | "image" ->
      let* data = parse_required_string server "data" value in
      let mime =
        Option.value
          (Jsonx.string_member "mimeType" value)
          ~default:"application/octet-stream"
      in
      Lwt.return_ok (Image { mime; data })
  | "resource" -> (
      match Jsonx.object_member "resource" value with
      | None ->
          Lwt.return_error
            (`Transport (server.name, "MCP resource content is missing resource"))
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
              Lwt.return_ok (Image { mime; data })
          | None -> Lwt.return_ok (Resource { uri; text })))
  | other ->
      Lwt.return_error
        (`Transport (server.name, Fmt.str "unsupported MCP content type %s" other))

let contents_of_result server value =
  match Jsonx.array_member "content" value with
  | None -> Lwt.return_error (`Transport (server.name, "MCP result is missing content"))
  | Some values ->
      let rec loop acc = function
        | [] -> Lwt.return_ok (List.rev acc)
        | value :: rest ->
            let* parsed = content_of_json server value in
            loop (parsed :: acc) rest
      in
      loop [] values

let call t ~server ~tool ~input =
  match find_server t server with
  | None -> Lwt.return_error (`Unknown_server server)
  | Some server ->
      let params = json_object [ ("name", json_string tool); ("arguments", input) ] in
      let* result = rpc server ~method_name:"tools/call" ~params in
      let* content = contents_of_result server result in
      Lwt.return_ok
        (content, Option.value (Jsonx.bool_member "isError" result) ~default:false)

let read_resource t ~server ~uri =
  match find_server t server with
  | None -> Lwt.return_error (`Unknown_server server)
  | Some server -> (
      let params = json_object [ ("uri", json_string uri) ] in
      let* result = rpc server ~method_name:"resources/read" ~params in
      match Jsonx.array_member "contents" result with
      | None ->
          Lwt.return_error
            (`Transport (server.name, "MCP resources/read result is missing contents"))
      | Some values ->
          let rec loop acc = function
            | [] -> Lwt.return_ok (List.rev acc)
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
                  | Some data, _, _ -> Lwt.return_ok (Image { mime; data })
                  | None, Some text, Some uri ->
                      Lwt.return_ok (Resource { uri; text = Some text })
                  | None, None, Some uri -> Lwt.return_ok (Resource { uri; text = None })
                  | _ ->
                      Lwt.return_error
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
  | Some (Jsont.Array (values, _)) ->
      let rec loop acc = function
        | [] -> Lwt.return_ok (content_text (List.rev acc))
        | value :: rest ->
            let* parsed = content_of_json server value in
            loop (parsed :: acc) rest
      in
      loop [] values
  | Some content ->
      let* parsed = content_of_json server content in
      Lwt.return_ok (content_text [ parsed ])
  | None ->
      Lwt.return_error (`Transport (server.name, "MCP prompt message is missing content"))

let get_prompt t ~server ~name ~args =
  match find_server t server with
  | None -> Lwt.return_error (`Unknown_server server)
  | Some server -> (
      let arguments =
        args
        |> List.map (fun (key, value) ->
            Jsont.Json.mem (Jsont.Json.name key) (json_string value))
        |> Jsont.Json.object'
      in
      let params = json_object [ ("name", json_string name); ("arguments", arguments) ] in
      let* result = rpc server ~method_name:"prompts/get" ~params in
      match Jsonx.array_member "messages" result with
      | None ->
          Lwt.return_error
            (`Transport (server.name, "MCP prompts/get result is missing messages"))
      | Some values ->
          let rec loop acc = function
            | [] -> Lwt.return_ok (String.concat "\n\n" (List.rev acc))
            | value :: rest ->
                let* text = prompt_message_text server value in
                loop (text :: acc) rest
          in
          loop [] values)

let close t =
  if t.closed then Lwt.return_unit
  else (
    t.closed <- true;
    List.iter Lwt.cancel t.fibers;
    Lwt.join (List.map close_server t.servers))
