let log_src = Logs.Src.create "crush.lsp"

module Log = (val Logs.src_log log_src : Logs.LOG)
open Lwt.Infix
open Result.Syntax

type diagnostic = {
  path : string;
  line : int;
  col : int;
  severity : [ `Error | `Warning | `Info | `Hint ];
  message : string;
  source : string option;
}

type location = { path : string; line : int; col : int; end_line : int; end_col : int }
type symbol = { name : string; kind : string; range : location; children : symbol list }
type text_edit = { range : location; new_text : string }
type server_state = Not_started | Starting | Ready | Failed of string | Disabled

type error =
  [ `No_server of string
  | `Not_ready of string
  | `Rpc of string * string
  | `Timeout of string
  | `Io of string * string ]

type rpc_reply = [ `Result of Jsont.json | `Error of int * string ]
type pending = { condition : unit Lwt_condition.t; mutable reply : rpc_reply option }

type document = {
  uri : string;
  language_id : string;
  mutable version : int;
  mutable text : string;
  mutable opened : bool;
}

type diagnostic_state = { mutable values : diagnostic list; mutable serial : int }

type server = {
  name : string;
  config : Config.lsp;
  mutable state : server_state;
  mutable process : Charamel_os.Process.t option;
  mutable stdin : Lwt_io.output_channel option;
  mutable stdout : Lwt_io.input_channel option;
  mutable stderr : Lwt_io.input_channel option;
  mutable exit_status : int option;
  process_wait_lock : Lwt_mutex.t;
  mutable generation : int;
  mutable stopping : bool;
  mutable next_id : int;
  pending : (int, pending) Hashtbl.t;
  documents : (string, document) Hashtbl.t;
  state_condition : unit Lwt_condition.t;
  write_lock : Lwt_mutex.t;
}

type t = {
  clock : Charamel_os.Time.clock;
  fs_root : string;
  cwd : string;
  servers_table : server list;
  lock : Lwt_mutex.t;
  diagnostics_table : (string, diagnostic_state) Hashtbl.t;
}

let max_frame_size = 16 * 1024 * 1024

let pp_error ppf = function
  | `No_server path -> Fmt.pf ppf "no LSP server for %s" path
  | `Not_ready server -> Fmt.pf ppf "LSP server %s is not ready" server
  | `Rpc (server, message) -> Fmt.pf ppf "LSP %s: %s" server message
  | `Timeout server -> Fmt.pf ppf "LSP server %s timed out" server
  | `Io (path, message) -> Fmt.pf ppf "LSP I/O error for %s: %s" path message

let defaults =
  [
    ( "ocamllsp",
      {
        Config.command = "ocamllsp";
        args = [];
        filetypes = [ "ml"; "mli" ];
        root_markers = [ "dune-project"; "*.opam" ];
        init_options = None;
      } );
    ( "gopls",
      {
        Config.command = "gopls";
        args = [];
        filetypes = [ "go" ];
        root_markers = [ "go.mod" ];
        init_options = None;
      } );
    ( "rust-analyzer",
      {
        Config.command = "rust-analyzer";
        args = [];
        filetypes = [ "rs" ];
        root_markers = [ "Cargo.toml" ];
        init_options = None;
      } );
    ( "typescript-language-server",
      {
        Config.command = "typescript-language-server";
        args = [ "--stdio" ];
        filetypes = [ "ts"; "tsx"; "js"; "jsx" ];
        root_markers = [ "package.json"; "tsconfig.json" ];
        init_options = None;
      } );
    ( "pyright-langserver",
      {
        Config.command = "pyright-langserver";
        args = [ "--stdio" ];
        filetypes = [ "py"; "pyi" ];
        root_markers = [ "pyproject.toml"; "setup.py"; "requirements.txt" ];
        init_options = None;
      } );
  ]

let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.number (float_of_int value)
let json_bool value = Jsont.Json.bool value
let json_null () = Jsont.Json.null ()
let json_array values = Jsont.Json.list values

let json_object fields =
  Jsont.Json.object'
    (List.map (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value) fields)

let percent_hex value =
  let digits = "0123456789ABCDEF" in
  String.init 2 (fun index -> digits.[(value lsr ((1 - index) * 4)) land 0xF])

let uri_escape path =
  let buffer = Buffer.create (String.length path + 8) in
  String.iter
    (fun character ->
      let code = Char.code character in
      if
        (code >= Char.code 'a' && code <= Char.code 'z')
        || (code >= Char.code 'A' && code <= Char.code 'Z')
        || (code >= Char.code '0' && code <= Char.code '9')
        || List.mem character [ '/'; '-'; '_'; '.'; '~' ]
      then Buffer.add_char buffer character
      else (
        Buffer.add_char buffer '%';
        Buffer.add_string buffer (percent_hex code)))
    path;
  Buffer.contents buffer

let hex_digit character =
  if character >= '0' && character <= '9' then Some (Char.code character - Char.code '0')
  else if character >= 'a' && character <= 'f' then
    Some (Char.code character - Char.code 'a' + 10)
  else if character >= 'A' && character <= 'F' then
    Some (Char.code character - Char.code 'A' + 10)
  else None

let uri_unescape text =
  let buffer = Buffer.create (String.length text) in
  let rec loop index =
    if index >= String.length text then Ok (Buffer.contents buffer)
    else if text.[index] = '%' && index + 2 < String.length text then
      match (hex_digit text.[index + 1], hex_digit text.[index + 2]) with
      | Some high, Some low ->
          Buffer.add_char buffer (Char.chr ((high lsl 4) lor low));
          loop (index + 3)
      | _ -> Error "invalid percent escape"
    else if text.[index] = '%' then Error "truncated percent escape"
    else (
      Buffer.add_char buffer text.[index];
      loop (index + 1))
  in
  loop 0

let path_of_uri uri =
  let raw =
    if String.starts_with ~prefix:"file://" uri then
      let path = String.sub uri 7 (String.length uri - 7) in
      if String.starts_with ~prefix:"localhost/" path then
        String.sub path 9 (String.length path - 9)
      else path
    else uri
  in
  match uri_unescape raw with Ok path -> path | Error _ -> raw

let uri_of_path path = "file://" ^ uri_escape path

let extension path =
  let base =
    match String.rindex_opt path '/' with
    | None -> path
    | Some index -> String.sub path (index + 1) (String.length path - index - 1)
  in
  match String.rindex_opt base '.' with
  | Some index when index > 0 && index + 1 < String.length base ->
      String.lowercase_ascii
        (String.sub base (index + 1) (String.length base - index - 1))
  | _ -> ""

let marker_matches fs_root directory marker =
  try
    if String.starts_with ~prefix:"*." marker then
      let suffix = String.sub marker 1 (String.length marker - 1) in
      Array.exists
        (fun entry ->
          String.length entry >= String.length suffix && String.ends_with ~suffix entry)
        (Sys.readdir (Filename.concat fs_root directory))
    else Sys.file_exists (Filename.concat (Filename.concat fs_root directory) marker)
  with Unix.Unix_error _ | Sys_error _ -> false

let has_root_marker t server =
  match server.config.Config.root_markers with
  | [] -> true
  | markers ->
      let rec check directory =
        if List.exists (marker_matches t.fs_root directory) markers then true
        else
          match Path.parent directory with None -> false | Some parent -> check parent
      in
      check t.cwd

let extension_supported server ext =
  List.exists
    (fun value -> String.lowercase_ascii value = ext)
    server.config.Config.filetypes

let server_for_path t path =
  let path = Path.normalize ~cwd:t.cwd path in
  if not (Path.within ~root:t.cwd path) then None
  else
    let ext = extension path in
    List.find_opt (fun server -> extension_supported server ext) t.servers_table

let find_server_by_name t name =
  List.find_opt (fun server -> server.name = name) t.servers_table

let set_state t server state =
  Lwt_mutex.with_lock t.lock (fun () ->
      server.state <- state;
      Lwt_condition.broadcast server.state_condition ();
      Lwt.return_unit)

let mark_pending server reply =
  let pending = Hashtbl.fold (fun _ value acc -> value :: acc) server.pending [] in
  Hashtbl.reset server.pending;
  List.iter
    (fun pending ->
      if Option.is_none pending.reply then pending.reply <- Some reply;
      Lwt_condition.broadcast pending.condition ())
    pending

let fail_server t server failure_message =
  Lwt_mutex.with_lock t.lock (fun () ->
      let process = server.process in
      (match server.state with
      | Not_started | Disabled -> ()
      | Starting | Ready -> server.state <- Failed failure_message
      | Failed _ -> ());
      mark_pending server (`Error (-32000, failure_message));
      Lwt_condition.broadcast server.state_condition ();
      Lwt.return process)
  >>= function
  | None -> Lwt.return_unit
  | Some process ->
      Log.debug (fun m -> m "server %s failed: %s" server.name failure_message);
      (try Charamel_os.Process.terminate process
       with Unix.Unix_error (Unix.EPERM, _, _) -> ());
      Lwt.return_unit

let parse_content_length line =
  match String.index_opt line ':' with
  | None -> Error "header has no colon"
  | Some index -> (
      let key = String.lowercase_ascii (String.trim (String.sub line 0 index)) in
      let value =
        String.trim (String.sub line (index + 1) (String.length line - index - 1))
      in
      if key <> "content-length" then Ok None
      else
        try
          let length = int_of_string value in
          if length >= 0 && length <= max_frame_size then Ok (Some length)
          else Error "invalid Content-Length"
        with Failure _ -> Error "invalid Content-Length")

let read_exact channel length =
  let buffer = Bytes.create length in
  let rec fill offset =
    if offset = length then Lwt.return buffer
    else
      Lwt_io.read_into channel buffer offset (length - offset) >>= fun read ->
      if read = 0 then Lwt.fail End_of_file else fill (offset + read)
  in
  fill 0 >|= Bytes.to_string

let read_frame channel =
  let rec read_headers length =
    Lwt_io.read_line channel >>= fun line ->
    if String.length line = 0 then
      match length with
      | Some length -> Lwt.return_ok length
      | None -> Lwt.return_error "missing Content-Length"
    else
      match parse_content_length line with
      | Error message -> Lwt.return_error message
      | Ok None -> read_headers length
      | Ok (Some value) -> (
          match length with
          | Some previous when previous <> value ->
              Lwt.return_error "duplicate Content-Length"
          | _ -> read_headers (Some value))
  in
  read_headers None >>= function
  | Error message -> Lwt.return_error message
  | Ok length ->
      Lwt.catch
        (fun () ->
          read_exact channel length >|= fun body ->
          match Jsonx.json_of_string body with
          | Ok value -> Ok value
          | Error message -> Error ("invalid JSON: " ^ message))
        (function
          | End_of_file -> Lwt.return_error "truncated JSON frame" | exn -> Lwt.fail exn)

let send_json server value =
  let payload = Jsonx.string_of_json value in
  let frame = Fmt.str "Content-Length: %d\r\n\r\n%s" (String.length payload) payload in
  Lwt_mutex.with_lock server.write_lock (fun () ->
      match server.stdin with
      | None -> Lwt.return_error "server input is closed"
      | Some sink ->
          Lwt.catch
            (fun () ->
              Lwt_io.write sink frame >>= fun () ->
              Lwt_io.flush sink >|= fun () -> Ok ())
            (function
              | End_of_file -> Lwt.return_error "server input reached EOF"
              | Lwt_io.Channel_closed where ->
                  Lwt.return_error (Fmt.str "server input is closed: %s" where)
              | Unix.Unix_error _ as exn -> Lwt.return_error (Io.message exn)
              | exn -> Lwt.fail exn))

let notification server ~method_ ~params =
  send_json server
    (json_object
       [
         ("jsonrpc", json_string "2.0");
         ("method", json_string method_);
         ("params", params);
       ])

let cancel_request server id =
  send_json server
    (json_object
       [
         ("jsonrpc", json_string "2.0");
         ("method", json_string "$/cancelRequest");
         ("params", json_object [ ("id", json_int id) ]);
       ])
  >|= ignore

let next_request server =
  let id = server.next_id in
  server.next_id <- id + 1;
  id

let request t server ~timeout ~method_ ~params =
  let pending = { condition = Lwt_condition.create (); reply = None } in
  Lwt_mutex.with_lock t.lock (fun () ->
      let id = next_request server in
      Hashtbl.replace server.pending id pending;
      Lwt.return id)
  >>= fun id ->
  let request_json =
    json_object
      [
        ("jsonrpc", json_string "2.0");
        ("id", json_int id);
        ("method", json_string method_);
        ("params", params);
      ]
  in
  let remove_pending () =
    Lwt_mutex.with_lock t.lock (fun () ->
        Hashtbl.remove server.pending id;
        Lwt.return_unit)
  in
  send_json server request_json >>= function
  | Error message ->
      remove_pending () >>= fun () ->
      fail_server t server message >>= fun () ->
      Lwt.return_error (`Rpc (server.name, message))
  | Ok () ->
      let wait_reply () =
        Lwt_mutex.with_lock t.lock (fun () ->
            let rec wait () =
              match pending.reply with
              | Some reply -> Lwt.return reply
              | None -> Lwt_condition.wait ~mutex:t.lock pending.condition >>= wait
            in
            wait ())
      in
      Lwt.catch
        (fun () ->
          Lwt_unix.with_timeout timeout wait_reply >>= fun reply ->
          remove_pending () >>= fun () ->
          match reply with
          | `Result value -> Lwt.return_ok value
          | `Error (code, message) ->
              Lwt.return_error (`Rpc (server.name, Fmt.str "[%d] %s" code message)))
        (function
          | Lwt_unix.Timeout ->
              remove_pending () >>= fun () ->
              cancel_request server id >>= fun () ->
              Lwt.return_error (`Timeout server.name)
          | Lwt.Canceled ->
              remove_pending () >>= fun () ->
              cancel_request server id >>= fun () -> Lwt.fail Lwt.Canceled
          | exn -> Lwt.fail exn)

let response_id value = Jsonx.int_member "id" value

let diagnostic_severity = function
  | Some 1 -> `Error
  | Some 2 -> `Warning
  | Some 3 -> `Info
  | Some 4 -> `Hint
  | _ -> `Info

let position_of_json value =
  match (Jsonx.int_member "line" value, Jsonx.int_member "character" value) with
  | Some line, Some character when line >= 0 && character >= 0 -> Ok (line, character)
  | _ -> Error "invalid LSP position"

let range_json value =
  match (Jsonx.member "start" value, Jsonx.member "end" value) with
  | Some start, Some finish ->
      let* start_line, start_col = position_of_json start in
      let* end_line, end_col = position_of_json finish in
      if end_line < start_line || (end_line = start_line && end_col < start_col) then
        Error "LSP range is reversed"
      else Ok (start_line, start_col, end_line, end_col)
  | _ -> Error "missing LSP range"

let location_of_json ~cwd ~server value =
  let uri =
    match (Jsonx.string_member "uri" value, Jsonx.string_member "targetUri" value) with
    | Some uri, _ -> Some uri
    | None, Some uri -> Some uri
    | None, None -> None
  in
  match (uri, Jsonx.member "range" value, Jsonx.member "targetRange" value) with
  | Some uri, Some range, _ | Some uri, None, Some range -> (
      match range_json range with
      | Error message -> Error (`Rpc (server.name, message))
      | Ok (line, col, end_line, end_col) ->
          Ok
            {
              path = Path.normalize ~cwd (path_of_uri uri);
              line = line + 1;
              col = col + 1;
              end_line = end_line + 1;
              end_col = end_col + 1;
            })
  | _ -> Error (`Rpc (server.name, "location has no URI or range"))

let locations_of_result ~cwd server value =
  match value with
  | Jsont.Null _ -> Ok []
  | Jsont.Array (values, _) ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* location = location_of_json ~cwd ~server value in
            collect (location :: acc) rest
      in
      collect [] values
  | Jsont.Object _ ->
      let* location = location_of_json ~cwd ~server value in
      Ok [ location ]
  | _ -> Error (`Rpc (server.name, "invalid location response"))

let symbol_kind_name = function
  | 1 -> "File"
  | 2 -> "Module"
  | 3 -> "Namespace"
  | 4 -> "Package"
  | 5 -> "Class"
  | 6 -> "Method"
  | 7 -> "Property"
  | 8 -> "Field"
  | 9 -> "Constructor"
  | 10 -> "Enum"
  | 11 -> "Interface"
  | 12 -> "Function"
  | 13 -> "Variable"
  | 14 -> "Constant"
  | 15 -> "String"
  | 16 -> "Number"
  | 17 -> "Boolean"
  | 18 -> "Array"
  | 19 -> "Object"
  | 20 -> "Key"
  | 21 -> "Null"
  | 22 -> "EnumMember"
  | 23 -> "Struct"
  | 24 -> "Event"
  | 25 -> "Operator"
  | 26 -> "TypeParameter"
  | value -> Fmt.str "Kind%d" value

let rec symbol_of_json ~cwd ~server ~path ~depth value =
  if depth > 128 then Error (`Rpc (server.name, "symbol nesting exceeds limit"))
  else
    let* name =
      match Jsonx.string_member "name" value with
      | Some name -> Ok name
      | None -> Error (`Rpc (server.name, "symbol has no name"))
    in
    let location_value = Option.value ~default:value (Jsonx.member "location" value) in
    let* range =
      match Jsonx.member "range" location_value with
      | None -> Error (`Rpc (server.name, "symbol has no range"))
      | Some range -> (
          match range_json range with
          | Error message -> Error (`Rpc (server.name, message))
          | Ok (line, col, end_line, end_col) ->
              let range_path =
                match Jsonx.string_member "uri" location_value with
                | Some uri -> Path.normalize ~cwd (path_of_uri uri)
                | None -> path
              in
              Ok
                {
                  path = range_path;
                  line = line + 1;
                  col = col + 1;
                  end_line = end_line + 1;
                  end_col = end_col + 1;
                })
    in
    let kind =
      match (Jsonx.string_member "kind" value, Jsonx.int_member "kind" value) with
      | Some kind, _ -> kind
      | None, Some kind -> symbol_kind_name kind
      | None, None -> "Unknown"
    in
    let* children =
      match Jsonx.member "children" value with
      | None -> Ok []
      | Some (Jsont.Array (values, _)) ->
          let rec collect acc = function
            | [] -> Ok (List.rev acc)
            | value :: rest ->
                let* symbol =
                  symbol_of_json ~cwd ~server ~path ~depth:(depth + 1) value
                in
                collect (symbol :: acc) rest
          in
          collect [] values
      | Some _ -> Error (`Rpc (server.name, "symbol children is not an array"))
    in
    Ok { name; kind; range; children }

let symbols_of_result ~cwd server ~path value =
  match value with
  | Jsont.Null _ -> Ok []
  | Jsont.Array (values, _) ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* symbol = symbol_of_json ~cwd ~server ~path ~depth:0 value in
            collect (symbol :: acc) rest
      in
      collect [] values
  | _ -> Error (`Rpc (server.name, "invalid document symbols response"))

let parse_diagnostic ~path value =
  let* range =
    match Jsonx.member "range" value with
    | None -> Error "diagnostic has no range"
    | Some range -> range_json range
  in
  let* message =
    match Jsonx.string_member "message" value with
    | Some text -> Ok text
    | None -> Error "diagnostic has no message"
  in
  let source = Jsonx.string_member "source" value in
  let severity = diagnostic_severity (Jsonx.int_member "severity" value) in
  let line, col, _, _ = range in
  Ok { path; line = line + 1; col = col + 1; severity; message; source }

let update_diagnostics t value =
  match Jsonx.member "params" value with
  | Some params -> (
      match Jsonx.string_member "uri" params with
      | None -> Lwt.return_unit
      | Some uri ->
          let path = Path.normalize ~cwd:t.cwd (path_of_uri uri) in
          let values =
            match Jsonx.member "diagnostics" params with
            | Some (Jsont.Array (diagnostics, _)) ->
                List.fold_left
                  (fun acc value ->
                    match parse_diagnostic ~path value with
                    | Ok diagnostic -> diagnostic :: acc
                    | Error _ -> acc)
                  [] diagnostics
                |> List.rev
            | _ -> []
          in
          Lwt_mutex.with_lock t.lock (fun () ->
              let state =
                match Hashtbl.find_opt t.diagnostics_table path with
                | Some state -> state
                | None ->
                    let state = { values = []; serial = 0 } in
                    Hashtbl.add t.diagnostics_table path state;
                    state
              in
              state.values <- values;
              state.serial <- state.serial + 1;
              Lwt.return_unit))
  | None -> Lwt.return_unit

let send_response server ~id value =
  send_json server
    (json_object [ ("jsonrpc", json_string "2.0"); ("id", id); ("result", value) ])
  >|= ignore

let send_error_response server ~id code message =
  send_json server
    (json_object
       [
         ("jsonrpc", json_string "2.0");
         ("id", id);
         ( "error",
           json_object [ ("code", json_int code); ("message", json_string message) ] );
       ])
  >|= ignore

let handle_response t server value =
  match response_id value with
  | None -> fail_server t server "response has no request id"
  | Some id ->
      let reply =
        match Jsonx.member "error" value with
        | Some error ->
            let code = Option.value ~default:(-32000) (Jsonx.int_member "code" error) in
            let message =
              Option.value ~default:"unknown LSP error"
                (Jsonx.string_member "message" error)
            in
            `Error (code, message)
        | None -> (
            match Jsonx.member "result" value with
            | Some result -> `Result result
            | None -> `Error (-32603, "response has no result"))
      in
      Lwt_mutex.with_lock t.lock (fun () ->
          (match Hashtbl.find_opt server.pending id with
          | None -> ()
          | Some pending ->
              pending.reply <- Some reply;
              Lwt_condition.broadcast pending.condition ());
          Lwt.return_unit)

let handle_server_request server ~id ~method_ =
  if method_ = "workspace/configuration" then send_response server ~id (json_array [])
  else send_error_response server ~id (-32601) (Fmt.str "method not found: %s" method_)

let handle_message t server value =
  match Jsonx.object_members value with
  | None -> fail_server t server "JSON-RPC message is not an object"
  | Some _ -> (
      match Jsonx.string_member "method" value with
      | Some method_ -> (
          match Jsonx.member "id" value with
          | Some id -> handle_server_request server ~id ~method_
          | None ->
              if method_ = "textDocument/publishDiagnostics" then
                update_diagnostics t value
              else if method_ = "window/showMessage" then Lwt.return_unit
              else Lwt.return_unit)
      | None -> handle_response t server value)

let reader_loop t server generation source =
  let fail message =
    if server.generation = generation then fail_server t server message
    else Lwt.return_unit
  in
  let rec loop () =
    read_frame source >>= function
    | Error message -> fail message
    | Ok value -> handle_message t server value >>= loop
  in
  Lwt.catch loop (function
    | End_of_file -> fail "server EOF"
    | Lwt_io.Channel_closed _ -> fail "server stream closed"
    | Unix.Unix_error _ as exn -> fail (Io.message exn)
    | Failure message -> fail message
    | Lwt.Canceled -> fail "server reader cancelled"
    | exn -> Lwt.fail exn)

let stderr_loop source =
  let chunk = Bytes.create 4096 in
  let rec loop () =
    Lwt.catch
      (fun () -> Lwt_io.read_into source chunk 0 4096)
      (function
        | End_of_file | Lwt_io.Channel_closed _ | Unix.Unix_error _ | Sys_error _ ->
            Lwt.return 0
        | exn -> Lwt.fail exn)
    >>= fun read -> if read = 0 then Lwt.return_unit else loop ()
  in
  loop ()

let await_process t server generation process =
  Lwt_mutex.with_lock server.process_wait_lock (fun () ->
      match server.exit_status with
      | Some status -> Lwt.return status
      | None ->
          Charamel_os.Process.await process >>= fun status ->
          Lwt_mutex.with_lock t.lock (fun () ->
              if server.generation = generation then server.exit_status <- Some status;
              Lwt.return_unit)
          >>= fun () -> Lwt.return status)

let monitor_process t server generation process =
  let failed message =
    if server.generation = generation && not server.stopping then
      fail_server t server message
    else Lwt.return_unit
  in
  Lwt.catch
    (fun () ->
      await_process t server generation process >>= fun _ -> failed "server exited")
    (function
      | Unix.Unix_error _ as exn -> failed (Io.message exn)
      | Lwt.Canceled -> Lwt.return_unit
      | exn -> Lwt.fail exn)

let initialize_params t server =
  let root_uri = uri_of_path t.cwd in
  let capabilities =
    json_object
      [
        ("textDocument", json_object [ ("publishDiagnostics", json_object []) ]);
        ("workspace", json_object [ ("applyEdit", json_bool true) ]);
      ]
  in
  json_object
    [
      ("processId", json_null ());
      ("rootUri", json_string root_uri);
      ("capabilities", capabilities);
      ( "workspaceFolders",
        json_array
          [
            json_object
              [
                ("uri", json_string root_uri);
                ("name", json_string (Filename.basename t.cwd));
              ];
          ] );
      ( "initializationOptions",
        Option.value ~default:(json_null ()) server.config.Config.init_options );
    ]

let ignore_closed promise =
  Lwt.catch
    (fun () -> promise)
    (function
      | Lwt_io.Channel_closed _ | Unix.Unix_error _ | Sys_error _ -> Lwt.return_unit
      | exn -> Lwt.fail exn)

let close_input = function
  | None -> Lwt.return_unit
  | Some (channel : Lwt_io.input_channel) -> ignore_closed (Lwt_io.close channel)

let close_output = function
  | None -> Lwt.return_unit
  | Some (channel : Lwt_io.output_channel) -> ignore_closed (Lwt_io.close channel)

let cleanup_streams server =
  Lwt_mutex.with_lock server.write_lock (fun () ->
      let stdin = server.stdin in
      let stdout = server.stdout in
      let stderr = server.stderr in
      server.stdin <- None;
      server.stdout <- None;
      server.stderr <- None;
      Lwt.join [ close_output stdin; close_input stdout; close_input stderr ])

let read_whole_file path = Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read

let start_server t server =
  let argv = server.config.Config.command :: server.config.Config.args in
  Lwt.catch
    (fun () ->
      let process =
        Charamel_os.Process.spawn ~cwd:t.cwd ~stdin:`Pipe ~stdout:`Pipe ~stderr:`Pipe argv
      in
      let stdin = Charamel_os.Process.stdin_w process in
      let stdout = Charamel_os.Process.stdout_r process in
      let stderr = Charamel_os.Process.stderr_r process in
      Lwt_mutex.with_lock t.lock (fun () ->
          server.process <- Some process;
          server.stdin <- Some stdin;
          server.stdout <- Some stdout;
          server.stderr <- Some stderr;
          server.exit_status <- None;
          server.generation <- server.generation + 1;
          server.stopping <- false;
          Lwt.return server.generation)
      >>= fun generation ->
      Lwt.async (fun () -> reader_loop t server generation stdout);
      Lwt.async (fun () -> stderr_loop stderr);
      Lwt.async (fun () -> monitor_process t server generation process);
      request t server ~timeout:30. ~method_:"initialize"
        ~params:(initialize_params t server)
      >>= function
      | Error error ->
          let message = Fmt.str "%a" pp_error error in
          fail_server t server message
      | Ok _ -> (
          notification server ~method_:"initialized" ~params:(json_object []) >>= function
          | Error message -> fail_server t server message
          | Ok () -> set_state t server Ready))
    (function
      | Lwt.Canceled ->
          set_state t server (Failed "server startup cancelled") >>= fun () ->
          Lwt.fail Lwt.Canceled
      | Unix.Unix_error _ as exn -> set_state t server (Failed (Io.message exn))
      | Invalid_argument message | Failure message -> set_state t server (Failed message)
      | exn -> Lwt.fail exn)

let wait_started t server =
  Lwt_mutex.with_lock t.lock (fun () ->
      let rec loop () =
        match server.state with
        | Ready -> Lwt.return_ok ()
        | Failed message -> Lwt.return_error (`Not_ready message)
        | Disabled -> Lwt.return_error (`Not_ready server.name)
        | Not_started -> Lwt.return_error (`Not_ready server.name)
        | Starting -> Lwt_condition.wait ~mutex:t.lock server.state_condition >>= loop
      in
      loop ())

let ensure_started t server =
  Lwt_mutex.with_lock t.lock (fun () ->
      match server.state with
      | Not_started ->
          server.state <- Starting;
          Lwt.return true
      | Starting | Ready | Failed _ | Disabled -> Lwt.return false)
  >>= fun start ->
  if start then
    start_server t server >>= fun () ->
    match server.state with
    | Ready -> Lwt.return_ok ()
    | Failed message -> Lwt.return_error (`Not_ready message)
    | _ -> Lwt.return_error (`Not_ready server.name)
  else
    Lwt.catch
      (fun () -> Lwt_unix.with_timeout 30. (fun () -> wait_started t server))
      (function
        | Lwt_unix.Timeout -> Lwt.return_error (`Not_ready server.name)
        | exn -> Lwt.fail exn)

let send_document t server path text =
  let ext = extension path in
  Lwt_mutex.with_lock t.lock (fun () ->
      let document =
        match Hashtbl.find_opt server.documents path with
        | Some document ->
            document.version <- (if document.opened then document.version + 1 else 1);
            document.text <- text;
            document
        | None ->
            let document =
              {
                uri = uri_of_path path;
                language_id = ext;
                version = 1;
                text;
                opened = false;
              }
            in
            Hashtbl.add server.documents path document;
            document
      in
      Lwt.return document)
  >>= fun document ->
  (if document.opened then
     notification server ~method_:"textDocument/didChange"
       ~params:
         (json_object
            [
              ( "textDocument",
                json_object
                  [
                    ("uri", json_string document.uri);
                    ("version", json_int document.version);
                  ] );
              ( "contentChanges",
                json_array [ json_object [ ("text", json_string document.text) ] ] );
            ])
   else
     notification server ~method_:"textDocument/didOpen"
       ~params:
         (json_object
            [
              ( "textDocument",
                json_object
                  [
                    ("uri", json_string document.uri);
                    ("languageId", json_string document.language_id);
                    ("version", json_int document.version);
                    ("text", json_string document.text);
                  ] );
            ]))
  >>= function
  | Ok () ->
      document.opened <- true;
      Lwt.return_unit
  | Error message -> fail_server t server message

let touch t ~path =
  let path = Path.normalize ~cwd:t.cwd path in
  match server_for_path t path with
  | None -> Lwt.return_unit
  | Some server when not (has_root_marker t server) -> Lwt.return_unit
  | Some server -> (
      Lwt.catch
        (fun () -> read_whole_file path >|= Option.some)
        (function
          | Unix.Unix_error _ | Sys_error _ -> Lwt.return_none | exn -> Lwt.fail exn)
      >>= function
      | None -> Lwt.return_unit
      | Some text -> (
          ensure_started t server >>= function
          | Ok () -> send_document t server path text
          | Error _ -> Lwt.return_unit))

let servers t =
  Lwt_mutex.with_lock t.lock (fun () ->
      Lwt.return (List.map (fun server -> (server.name, server.state)) t.servers_table))

let handles t ~path =
  Option.map
    (fun server -> server.name)
    (server_for_path t (Path.normalize ~cwd:t.cwd path))

let diagnostic_snapshot t path =
  match Hashtbl.find_opt t.diagnostics_table path with
  | None -> ([], 0)
  | Some state -> (state.values, state.serial)

let diagnostics t ~path ~wait =
  let path = Path.normalize ~cwd:t.cwd path in
  let initial_values, initial_serial = diagnostic_snapshot t path in
  if wait <= 0. then Lwt.return initial_values
  else
    let deadline = Charamel_os.Time.now t.clock +. max 0. (min 1. wait) in
    let first_publication = ref None in
    let serial = ref initial_serial in
    let rec loop () =
      let values, current_serial = diagnostic_snapshot t path in
      if current_serial <> !serial then (
        serial := current_serial;
        first_publication := Some (Charamel_os.Time.now t.clock));
      let now = Charamel_os.Time.now t.clock in
      if now >= deadline then Lwt.return values
      else
        match !first_publication with
        | Some published when now -. published >= 0.3 -> Lwt.return values
        | _ ->
            Charamel_os.Time.sleep t.clock (min 0.05 (max 0.001 (deadline -. now)))
            >>= loop
    in
    loop ()

let valid_position line col = line >= 1 && col >= 1

let position_params ~path ~line ~col =
  json_object
    [
      ("textDocument", json_object [ ("uri", json_string (uri_of_path path)) ]);
      ( "position",
        json_object [ ("line", json_int (line - 1)); ("character", json_int (col - 1)) ]
      );
    ]

let prepare_request t path =
  let path = Path.normalize ~cwd:t.cwd path in
  match server_for_path t path with
  | None -> Lwt.return_error (`No_server path)
  | Some server when not (has_root_marker t server) -> Lwt.return_error (`No_server path)
  | Some server -> (
      touch t ~path >>= fun () ->
      ensure_started t server >>= function
      | Ok () -> Lwt.return_ok (server, path)
      | Error _ as e -> Lwt.return e)

let definition t ~path ~line ~col =
  if not (valid_position line col) then
    Lwt.return_error (`Rpc ("client", "invalid position"))
  else
    prepare_request t path >>= function
    | Error _ as e -> Lwt.return e
    | Ok (server, path) -> (
        request t server ~timeout:10. ~method_:"textDocument/definition"
          ~params:(position_params ~path ~line ~col)
        >>= function
        | Error _ as e -> Lwt.return e
        | Ok value -> Lwt.return (locations_of_result ~cwd:t.cwd server value))

let references t ~path ~line ~col =
  if not (valid_position line col) then
    Lwt.return_error (`Rpc ("client", "invalid position"))
  else
    prepare_request t path >>= function
    | Error _ as e -> Lwt.return e
    | Ok (server, path) -> (
        let params =
          json_object
            [
              ("textDocument", json_object [ ("uri", json_string (uri_of_path path)) ]);
              ( "position",
                json_object
                  [ ("line", json_int (line - 1)); ("character", json_int (col - 1)) ] );
              ("context", json_object [ ("includeDeclaration", json_bool true) ]);
            ]
        in
        request t server ~timeout:10. ~method_:"textDocument/references" ~params
        >>= function
        | Error _ as e -> Lwt.return e
        | Ok value -> Lwt.return (locations_of_result ~cwd:t.cwd server value))

let document_symbols t ~path =
  prepare_request t path >>= function
  | Error _ as e -> Lwt.return e
  | Ok (server, path) -> (
      let params =
        json_object
          [ ("textDocument", json_object [ ("uri", json_string (uri_of_path path)) ]) ]
      in
      request t server ~timeout:10. ~method_:"textDocument/documentSymbol" ~params
      >>= function
      | Error _ as e -> Lwt.return e
      | Ok value -> Lwt.return (symbols_of_result ~cwd:t.cwd server ~path value))

let rec find_exact name (symbols : symbol list) =
  match symbols with
  | [] -> None
  | symbol :: rest -> (
      if symbol.name = name then Some symbol
      else
        match find_exact name symbol.children with
        | Some _ as result -> result
        | None -> find_exact name rest)

let qualified_suffix name symbol_name =
  symbol_name = name
  || String.ends_with ~suffix:("." ^ name) symbol_name
  || String.ends_with ~suffix:("::" ^ name) symbol_name

let rec find_suffix name (symbols : symbol list) =
  match symbols with
  | [] -> None
  | symbol :: rest -> (
      if qualified_suffix name symbol.name then Some symbol
      else
        match find_suffix name symbol.children with
        | Some _ as result -> result
        | None -> find_suffix name rest)

let find_symbol t ~path ~name =
  document_symbols t ~path >>= function
  | Error _ as e -> Lwt.return e
  | Ok symbols -> (
      match find_exact name symbols with
      | Some symbol -> Lwt.return_ok (Some symbol)
      | None -> Lwt.return_ok (find_suffix name symbols))

let current_version server path =
  Option.map (fun document -> document.version) (Hashtbl.find_opt server.documents path)

let parse_text_edit server value =
  let* range =
    match Jsonx.member "range" value with
    | None -> Error (`Rpc (server.name, "text edit has no range"))
    | Some range -> (
        match range_json range with
        | Error message -> Error (`Rpc (server.name, message))
        | Ok (line, col, end_line, end_col) ->
            Ok
              {
                path = "";
                line = line + 1;
                col = col + 1;
                end_line = end_line + 1;
                end_col = end_col + 1;
              })
  in
  let* new_text =
    match Jsonx.string_member "newText" value with
    | Some text when String.is_valid_utf_8 text -> Ok text
    | Some _ -> Error (`Rpc (server.name, "text edit is not valid UTF-8"))
    | None -> Error (`Rpc (server.name, "text edit has no newText"))
  in
  Ok { range; new_text }

let parse_file_edits server path value =
  match Jsonx.array_members value with
  | None -> Error (`Rpc (server.name, "workspace edits must be arrays"))
  | Some values ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* edit = parse_text_edit server value in
            collect ({ edit with range = { edit.range with path } } :: acc) rest
      in
      collect [] values

let merge_group path edits groups =
  let rec loop = function
    | [] -> [ (path, edits) ]
    | (existing, values) :: rest when existing = path ->
        (existing, values @ edits) :: rest
    | item :: rest -> item :: loop rest
  in
  loop groups

let parse_workspace_edit ~cwd server value =
  let groups = ref [] in
  let parse_changes changes =
    match Jsonx.object_members changes with
    | None -> Error (`Rpc (server.name, "workspace changes is not an object"))
    | Some members ->
        let rec loop = function
          | [] -> Ok ()
          | ((name, _), edits) :: rest ->
              let path = Path.normalize ~cwd (path_of_uri name) in
              let* values = parse_file_edits server path edits in
              groups := merge_group path values !groups;
              loop rest
        in
        loop members
  in
  let parse_document_changes changes =
    match Jsonx.array_members changes with
    | None -> Error (`Rpc (server.name, "documentChanges is not an array"))
    | Some values ->
        let rec loop = function
          | [] -> Ok ()
          | value :: rest ->
              let* document =
                match Jsonx.member "textDocument" value with
                | Some document -> Ok document
                | None ->
                    Error (`Rpc (server.name, "resource operations are not supported"))
              in
              let* uri =
                match Jsonx.string_member "uri" document with
                | Some uri -> Ok uri
                | None -> Error (`Rpc (server.name, "document change has no URI"))
              in
              let path = Path.normalize ~cwd (path_of_uri uri) in
              let* () =
                match Jsonx.int_member "version" document with
                | Some version -> (
                    match current_version server path with
                    | Some current when current <> version ->
                        Error
                          (`Rpc
                             (server.name, "workspace edit has a stale document version"))
                    | _ -> Ok ())
                | None -> Ok ()
              in
              let* edits =
                match Jsonx.member "edits" value with
                | Some edits -> parse_file_edits server path edits
                | None -> Error (`Rpc (server.name, "document change has no edits"))
              in
              groups := merge_group path edits !groups;
              loop rest
        in
        loop values
  in
  let* () =
    match Jsonx.member "changes" value with
    | Some changes -> parse_changes changes
    | None -> Ok ()
  in
  let* () =
    match Jsonx.member "documentChanges" value with
    | Some changes -> parse_document_changes changes
    | None -> Ok ()
  in
  Ok !groups

let rename t ~path ~line ~col ~new_name =
  if not (valid_position line col) then
    Lwt.return_error (`Rpc ("client", "invalid position"))
  else if new_name = "" then Lwt.return_error (`Rpc ("client", "new name is empty"))
  else
    prepare_request t path >>= function
    | Error _ as e -> Lwt.return e
    | Ok (server, path) -> (
        let params =
          json_object
            [
              ("textDocument", json_object [ ("uri", json_string (uri_of_path path)) ]);
              ( "position",
                json_object
                  [ ("line", json_int (line - 1)); ("character", json_int (col - 1)) ] );
              ("newName", json_string new_name);
            ]
        in
        request t server ~timeout:10. ~method_:"textDocument/rename" ~params >>= function
        | Error _ as e -> Lwt.return e
        | Ok (Jsont.Null _) -> Lwt.return_ok []
        | Ok value -> Lwt.return (parse_workspace_edit ~cwd:t.cwd server value))

let line_starts text =
  let starts = ref [ 0 ] in
  String.iteri
    (fun index character -> if character = '\n' then starts := (index + 1) :: !starts)
    text;
  Array.of_list (List.rev !starts)

type line_bounds = { start_byte : int; content_end : int }

let get_line_bounds text starts line =
  if line < 1 || line > Array.length starts then Error "line is outside the file"
  else
    let start_byte = starts.(line - 1) in
    let full_end =
      if line = Array.length starts then String.length text else starts.(line)
    in
    let content_end =
      let end_byte =
        if full_end > start_byte && text.[full_end - 1] = '\n' then full_end - 1
        else full_end
      in
      if end_byte > start_byte && text.[end_byte - 1] = '\r' then end_byte - 1
      else end_byte
    in
    Ok { start_byte; content_end }

let utf16_to_byte text ~start ~stop units =
  if units < 0 then Error "negative UTF-16 position"
  else
    let rec loop byte consumed =
      if consumed = units then Ok byte
      else if byte >= stop then Error "UTF-16 position is outside the line"
      else
        let decode = String.get_utf_8_uchar text byte in
        let length = Uchar.utf_decode_length decode in
        if (not (Uchar.utf_decode_is_valid decode)) || byte + length > stop then
          Error "source is not valid UTF-8"
        else
          let width =
            if Uchar.to_int (Uchar.utf_decode_uchar decode) > 0xFFFF then 2 else 1
          in
          if consumed + width > units then Error "UTF-16 position splits a surrogate pair"
          else loop (byte + length) (consumed + width)
    in
    loop start 0

let location_bytes text starts location =
  let* first = get_line_bounds text starts location.line in
  let* last = get_line_bounds text starts location.end_line in
  let* start_byte =
    utf16_to_byte text ~start:first.start_byte ~stop:first.content_end (location.col - 1)
  in
  let* end_byte =
    utf16_to_byte text ~start:last.start_byte ~stop:last.content_end (location.end_col - 1)
  in
  if start_byte > first.content_end || end_byte > last.content_end then
    Error "edit position exceeds line bounds"
  else if location.line = location.end_line && end_byte < start_byte then
    Error "edit range is reversed"
  else Ok (start_byte, end_byte)

let compute_replacement path original edits =
  let starts = line_starts original in
  let ranges =
    List.map
      (fun edit ->
        match location_bytes original starts edit.range with
        | Ok (start_byte, end_byte) -> Ok (start_byte, end_byte, edit.new_text)
        | Error message -> Error (`Io (path, message)))
      edits
  in
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | Ok value :: rest -> collect (value :: acc) rest
    | Error error :: _ -> Error error
  in
  let* ranges = collect [] ranges in
  let sorted =
    List.sort
      (fun (left_start, left_end, _) (right_start, right_end, _) ->
        match Int.compare left_start right_start with
        | 0 -> Int.compare left_end right_end
        | result -> result)
      ranges
  in
  let rec validate previous_end = function
    | [] -> Ok ()
    | (start_byte, _end_byte, _) :: _ when start_byte < previous_end ->
        Error (`Io (path, "workspace edits overlap"))
    | (start_byte, end_byte, _) :: rest ->
        if end_byte < start_byte then Error (`Io (path, "workspace edit is reversed"))
        else validate end_byte rest
  in
  let* () = validate 0 sorted in
  let buffer = Buffer.create (String.length original + 64) in
  let cursor = ref 0 in
  List.iter
    (fun (start_byte, end_byte, new_text) ->
      Buffer.add_substring buffer original !cursor (start_byte - !cursor);
      Buffer.add_string buffer new_text;
      cursor := end_byte)
    sorted;
  Buffer.add_substring buffer original !cursor (String.length original - !cursor);
  Ok (Buffer.contents buffer)

let apply_file_edits ~cwd path edits =
  let path = Path.normalize ~cwd (path_of_uri path) in
  Lwt.catch
    (fun () -> read_whole_file path >|= fun text -> Ok text)
    (function
      | Unix.Unix_error _ as exn -> Lwt.return_error (`Io (path, Io.message exn))
      | Sys_error message -> Lwt.return_error (`Io (path, message))
      | exn -> Lwt.fail exn)
  >>= function
  | Error _ as e -> Lwt.return e
  | Ok original when not (String.is_valid_utf_8 original) ->
      Lwt.return_error (`Io (path, "source is not valid UTF-8"))
  | Ok original -> (
      match compute_replacement path original edits with
      | Error _ as e -> Lwt.return e
      | Ok replacement ->
          Lwt.catch
            (fun () ->
              Charamel_os.Fs.with_open_out ~perm:0o644 path (fun channel ->
                  Lwt_io.write channel replacement)
              >|= fun () -> Ok path)
            (function
              | Charamel_os.Fs.E (error, fs_path) ->
                  Lwt.return_error (`Io (fs_path, Io.fs_error error))
              | Unix.Unix_error _ as exn -> Lwt.return_error (`Io (path, Io.message exn))
              | exn -> Lwt.fail exn))

let apply_edits ~cwd grouped =
  let rec apply acc = function
    | [] -> Lwt.return_ok (List.rev acc)
    | (path, edits) :: rest -> (
        apply_file_edits ~cwd path edits >>= function
        | Error _ as e -> Lwt.return e
        | Ok touched -> apply (touched :: acc) rest)
  in
  apply [] grouped

let reopen_documents t server =
  Lwt_mutex.with_lock t.lock (fun () ->
      Lwt.return
        (Hashtbl.fold
           (fun path document acc -> (path, document.text) :: acc)
           server.documents []))
  >>= fun documents ->
  Lwt_list.iter_s (fun (path, text) -> send_document t server path text) documents

(* [stop] has already forced the group away; the status wait is bounded the same way, so a
   child the kernel will not reap cannot hold [stop_server] open forever. *)
let stop_process t server generation process =
  Charamel_os.Process.stop process >>= fun () ->
  Lwt.choose
    [
      (Lwt.protected (await_process t server generation process) >|= fun _ -> ());
      Lwt_unix.sleep Charamel_os.Process.grace;
    ]

let stop_server t server ~final =
  server.stopping <- true;
  let process = server.process in
  let generation = server.generation in
  (match server.state with
    | Ready ->
        request t server ~timeout:2. ~method_:"shutdown" ~params:(json_null ())
        >>= fun _ ->
        notification server ~method_:"exit" ~params:(json_null ()) >>= fun _ ->
        Lwt.return_unit
    | _ -> Lwt.return_unit)
  >>= fun () ->
  (match process with
    | None -> Lwt.return_unit
    | Some process -> stop_process t server generation process)
  >>= fun () ->
  cleanup_streams server >>= fun () ->
  Lwt_mutex.with_lock t.lock (fun () ->
      server.process <- None;
      Hashtbl.iter
        (fun _ document ->
          document.opened <- false;
          document.version <- 0)
        server.documents;
      mark_pending server (`Error (-32000, "server stopped"));
      if final then server.state <- Disabled else server.state <- Not_started;
      Lwt_condition.broadcast server.state_condition ();
      Lwt.return_unit)

let restart t ~name =
  match
    match name with
    | None -> Ok t.servers_table
    | Some name -> (
        match find_server_by_name t name with
        | Some server -> Ok [ server ]
        | None -> Error (`No_server name))
  with
  | Error _ as e -> Lwt.return e
  | Ok selected ->
      let restarted = ref [] and failed = ref [] in
      Lwt_list.iter_s
        (fun server ->
          stop_server t server ~final:false >>= fun () ->
          if not (has_root_marker t server) then (
            failed := server.name :: !failed;
            Lwt.return_unit)
          else
            ensure_started t server >>= fun _ ->
            match server.state with
            | Ready ->
                restarted := server.name :: !restarted;
                reopen_documents t server
            | Failed _ ->
                failed := server.name :: !failed;
                Lwt.return_unit
            | _ ->
                failed := server.name :: !failed;
                Lwt.return_unit)
        selected
      >>= fun () -> Lwt.return_ok (List.rev !restarted, List.rev !failed)

let stop_all t =
  Lwt_list.iter_s (fun server -> stop_server t server ~final:true) t.servers_table

let create ~sw ~clock ~fs_root ~cwd ~config =
  (* A relative create-time cwd anchors at the process directory; every
     later normalization threads [t.cwd] explicitly. *)
  let cwd = Path.normalize ~cwd:(Sys.getcwd ()) cwd in
  let configured =
    let base = if config.Config.options.Config.auto_lsp then defaults else [] in
    let replace name value values =
      let rec loop acc = function
        | [] -> List.rev ((name, value) :: acc)
        | (existing, _) :: rest when existing = name ->
            List.rev_append acc ((name, value) :: rest)
        | item :: rest -> loop (item :: acc) rest
      in
      loop [] values
    in
    List.fold_left
      (fun values ((name, value) : string * Config.lsp) ->
        let value =
          match List.assoc_opt name defaults with
          | Some default when value.Config.filetypes = [] ->
              {
                value with
                filetypes = default.Config.filetypes;
                root_markers = default.Config.root_markers;
              }
          | _ -> value
        in
        replace name value values)
      base config.Config.lsp
  in
  let servers_table =
    List.map
      (fun (name, config) ->
        {
          name;
          config;
          state = Not_started;
          process = None;
          stdin = None;
          stdout = None;
          stderr = None;
          exit_status = None;
          process_wait_lock = Lwt_mutex.create ();
          generation = 0;
          stopping = false;
          next_id = 1;
          pending = Hashtbl.create 32;
          documents = Hashtbl.create 32;
          state_condition = Lwt_condition.create ();
          write_lock = Lwt_mutex.create ();
        })
      configured
  in
  let t =
    {
      clock;
      fs_root;
      cwd;
      servers_table;
      lock = Lwt_mutex.create ();
      diagnostics_table = Hashtbl.create 64;
    }
  in
  Lwt_switch.add_hook (Some sw) (fun () -> stop_all t);
  t
