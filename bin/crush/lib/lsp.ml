let log_src = Logs.Src.create "crush.lsp"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( let* ) = Result.bind

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
type pending = { condition : Eio.Condition.t; mutable reply : rpc_reply option }

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
  mutable process : Eio_unix.Process.ty Eio.Resource.t option;
  mutable stdin : [ Eio.Flow.sink_ty | Eio.Resource.close_ty ] Eio.Resource.t option;
  mutable stdout : [ Eio.Flow.source_ty | Eio.Resource.close_ty ] Eio.Resource.t option;
  mutable stderr : [ Eio.Flow.source_ty | Eio.Resource.close_ty ] Eio.Resource.t option;
  mutable exit_status : Eio.Process.exit_status option;
  process_wait_lock : Eio.Mutex.t;
  mutable generation : int;
  mutable stopping : bool;
  mutable next_id : int;
  pending : (int, pending) Hashtbl.t;
  documents : (string, document) Hashtbl.t;
  state_condition : Eio.Condition.t;
  write_lock : Eio.Mutex.t;
}

type t = {
  sw : Eio.Switch.t;
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  cwd : string;
  servers_table : server list;
  lock : Eio.Mutex.t;
  diagnostics_table : (string, diagnostic_state) Hashtbl.t;
}

let max_frame_size = 16 * 1024 * 1024
let max_stderr_size = 1024 * 1024

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

let string_member name value = Jsonx.string_member name value
let int_member name value = Jsonx.int_member name value
let member name value = Jsonx.member name value

let object_value value =
  match value with Jsont.Object (members, _) -> Some members | _ -> None

let array_value value =
  match value with Jsont.Array (values, _) -> Some values | _ -> None

let trim_ascii text = String.trim text
let lowercase text = String.lowercase_ascii text

let starts_with ~prefix text =
  String.length text >= String.length prefix
  && String.sub text 0 (String.length prefix) = prefix

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
    if starts_with ~prefix:"file://" uri then
      let path = String.sub uri 7 (String.length uri - 7) in
      if starts_with ~prefix:"localhost/" path then
        String.sub path 9 (String.length path - 9)
      else path
    else uri
  in
  match uri_unescape raw with Ok path -> path | Error _ -> raw

let uri_of_path path = "file://" ^ uri_escape path

let normalize_path path =
  let absolute =
    if starts_with ~prefix:"/" path then path else Filename.concat (Sys.getcwd ()) path
  in
  let pieces = String.split_on_char '/' absolute in
  let result =
    List.fold_left
      (fun stack piece ->
        match piece with
        | "" | "." -> stack
        | ".." -> ( match stack with [] -> [] | _ :: tail -> tail)
        | _ -> piece :: stack)
      [] pieces
    |> List.rev
  in
  "/" ^ String.concat "/" result

let inside root path =
  let root = normalize_path root and path = normalize_path path in
  path = root || starts_with ~prefix:(root ^ "/") path

let parent_path path =
  let path = normalize_path path in
  match String.rindex_opt path '/' with
  | None | Some 0 -> "/"
  | Some index -> String.sub path 0 index

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

let marker_matches fs directory marker =
  try
    if starts_with ~prefix:"*." marker then
      let suffix = String.sub marker 1 (String.length marker - 1) in
      Eio.Path.read_dir Eio.Path.(fs / directory)
      |> List.exists (fun entry ->
          String.length entry >= String.length suffix && String.ends_with ~suffix entry)
    else
      match Eio.Path.kind ~follow:false Eio.Path.(fs / directory / marker) with
      | `Not_found -> false
      | _ -> true
  with
  | Eio.Io _ -> false
  | Unix.Unix_error _ -> false

let has_root_marker t server =
  match server.config.root_markers with
  | [] -> true
  | markers ->
      let rec check directory =
        if List.exists (marker_matches t.fs directory) markers then true
        else if directory = "/" then false
        else check (parent_path directory)
      in
      check t.cwd

let extension_supported server ext =
  List.exists (fun value -> lowercase value = ext) server.config.filetypes

let server_for_path t path =
  let path = normalize_path path in
  if not (inside t.cwd path) then None
  else
    let ext = extension path in
    List.find_opt (fun server -> extension_supported server ext) t.servers_table

let find_server_by_name t name =
  List.find_opt (fun server -> server.name = name) t.servers_table

let set_state t server state =
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      server.state <- state;
      Eio.Condition.broadcast server.state_condition)

let mark_pending server reply =
  let pending = Hashtbl.fold (fun _ value acc -> value :: acc) server.pending [] in
  Hashtbl.reset server.pending;
  List.iter
    (fun pending ->
      if Option.is_none pending.reply then pending.reply <- Some reply;
      Eio.Condition.broadcast pending.condition)
    pending

let fail_server t server failure_message =
  let process =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        let process = server.process in
        (match server.state with
        | Not_started | Disabled -> ()
        | Starting | Ready -> server.state <- Failed failure_message
        | Failed _ -> ());
        mark_pending server (`Error (-32000, failure_message));
        Eio.Condition.broadcast server.state_condition;
        process)
  in
  Log.debug (fun m -> m "server %s failed: %s" server.name failure_message);
  Option.iter (fun process -> Eio.Process.signal process Sys.sigterm) process

let read_header_line reader = Eio.Buf_read.line reader

let parse_content_length line =
  match String.index_opt line ':' with
  | None -> Error "header has no colon"
  | Some index -> (
      let key = lowercase (trim_ascii (String.sub line 0 index)) in
      let value =
        trim_ascii (String.sub line (index + 1) (String.length line - index - 1))
      in
      if key <> "content-length" then Ok None
      else
        try
          let length = int_of_string value in
          if length >= 0 && length <= max_frame_size then Ok (Some length)
          else Error "invalid Content-Length"
        with Failure _ -> Error "invalid Content-Length")

let read_frame reader =
  let rec read_headers length =
    let line = read_header_line reader in
    if line = "" then
      match length with
      | Some length -> Ok length
      | None -> Error "missing Content-Length"
    else
      match parse_content_length line with
      | Error message -> Error message
      | Ok None -> read_headers length
      | Ok (Some value) -> (
          match length with
          | Some previous when previous <> value -> Error "duplicate Content-Length"
          | _ -> read_headers (Some value))
  in
  match read_headers None with
  | Error message -> Error message
  | Ok length -> (
      try
        let body = Eio.Buf_read.take length reader in
        match Jsonx.json_of_string body with
        | Ok value -> Ok value
        | Error message -> Error ("invalid JSON: " ^ message)
      with
      | Eio.Buf_read.Buffer_limit_exceeded -> Error "JSON frame exceeds the buffer limit"
      | End_of_file -> Error "truncated JSON frame")

let send_json server value =
  let payload = Jsonx.string_of_json value in
  let frame = Fmt.str "Content-Length: %d\r\n\r\n%s" (String.length payload) payload in
  Eio.Mutex.use_rw ~protect:true server.write_lock (fun () ->
      match server.stdin with
      | None -> Error "server input is closed"
      | Some sink -> (
          try
            Eio.Flow.copy_string frame sink;
            Ok ()
          with
          | Eio.Io (_, _) as exception_ -> Error (Fmt.str "%a" Eio.Exn.pp exception_)
          | Unix.Unix_error (error, function_name, argument) ->
              Error
                (Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument)
          | End_of_file -> Error "server input reached EOF"))

let notification server ~method_ ~params =
  send_json server
    (json_object
       [
         ("jsonrpc", json_string "2.0");
         ("method", json_string method_);
         ("params", params);
       ])

let cancel_request server id =
  Eio.Cancel.protect (fun () ->
      ignore
        (send_json server
           (json_object
              [
                ("jsonrpc", json_string "2.0");
                ("method", json_string "$/cancelRequest");
                ("params", json_object [ ("id", json_int id) ]);
              ])))

let next_request server =
  let id = server.next_id in
  server.next_id <- id + 1;
  id

let request t server ~timeout ~method_ ~params =
  let pending = { condition = Eio.Condition.create (); reply = None } in
  let id =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        let id = next_request server in
        Hashtbl.replace server.pending id pending;
        id)
  in
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
    Eio.Mutex.use_rw ~protect:true t.lock (fun () -> Hashtbl.remove server.pending id)
  in
  match send_json server request_json with
  | Error message ->
      remove_pending ();
      fail_server t server message;
      Error (`Rpc (server.name, message))
  | Ok () -> (
      let wait_reply () =
        Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
            let rec wait () =
              match pending.reply with
              | Some reply -> reply
              | None ->
                  Eio.Condition.await pending.condition t.lock;
                  wait ()
            in
            wait ())
      in
      let timed =
        try Eio.Time.with_timeout t.clock timeout (fun () -> Ok (wait_reply ()))
        with Eio.Cancel.Cancelled _ as exception_ ->
          remove_pending ();
          cancel_request server id;
          raise exception_
      in
      remove_pending ();
      match timed with
      | Error `Timeout ->
          cancel_request server id;
          Error (`Timeout server.name)
      | Ok (`Result value) -> Ok value
      | Ok (`Error (code, message)) ->
          Error (`Rpc (server.name, Fmt.str "[%d] %s" code message)))

let response_id value = int_member "id" value

let diagnostic_severity = function
  | Some 1 -> `Error
  | Some 2 -> `Warning
  | Some 3 -> `Info
  | Some 4 -> `Hint
  | _ -> `Info

let position_of_json value =
  match (int_member "line" value, int_member "character" value) with
  | Some line, Some character when line >= 0 && character >= 0 -> Ok (line, character)
  | _ -> Error "invalid LSP position"

let range_json value =
  match (member "start" value, member "end" value) with
  | Some start, Some finish ->
      let* start_line, start_col = position_of_json start in
      let* end_line, end_col = position_of_json finish in
      if end_line < start_line || (end_line = start_line && end_col < start_col) then
        Error "LSP range is reversed"
      else Ok (start_line, start_col, end_line, end_col)
  | _ -> Error "missing LSP range"

let location_of_json ~server value =
  let uri =
    match (string_member "uri" value, string_member "targetUri" value) with
    | Some uri, _ -> Some uri
    | None, Some uri -> Some uri
    | None, None -> None
  in
  match (uri, member "range" value, member "targetRange" value) with
  | Some uri, Some range, _ | Some uri, None, Some range -> (
      match range_json range with
      | Error message -> Error (`Rpc (server.name, message))
      | Ok (line, col, end_line, end_col) ->
          Ok
            {
              path = normalize_path (path_of_uri uri);
              line = line + 1;
              col = col + 1;
              end_line = end_line + 1;
              end_col = end_col + 1;
            })
  | _ -> Error (`Rpc (server.name, "location has no URI or range"))

let locations_of_result server value =
  match value with
  | Jsont.Null _ -> Ok []
  | Jsont.Array (values, _) ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* location = location_of_json ~server value in
            collect (location :: acc) rest
      in
      collect [] values
  | Jsont.Object _ ->
      let* location = location_of_json ~server value in
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

let rec symbol_of_json ~server ~path ~depth value =
  if depth > 128 then Error (`Rpc (server.name, "symbol nesting exceeds limit"))
  else
    let* name =
      match string_member "name" value with
      | Some name -> Ok name
      | None -> Error (`Rpc (server.name, "symbol has no name"))
    in
    let location_value = Option.value ~default:value (member "location" value) in
    let* range =
      match member "range" location_value with
      | None -> Error (`Rpc (server.name, "symbol has no range"))
      | Some range -> (
          match range_json range with
          | Error message -> Error (`Rpc (server.name, message))
          | Ok (line, col, end_line, end_col) ->
              let range_path =
                match string_member "uri" location_value with
                | Some uri -> normalize_path (path_of_uri uri)
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
      match (string_member "kind" value, int_member "kind" value) with
      | Some kind, _ -> kind
      | None, Some kind -> symbol_kind_name kind
      | None, None -> "Unknown"
    in
    let* children =
      match member "children" value with
      | None -> Ok []
      | Some (Jsont.Array (values, _)) ->
          let rec collect acc = function
            | [] -> Ok (List.rev acc)
            | value :: rest ->
                let* symbol = symbol_of_json ~server ~path ~depth:(depth + 1) value in
                collect (symbol :: acc) rest
          in
          collect [] values
      | Some _ -> Error (`Rpc (server.name, "symbol children is not an array"))
    in
    Ok { name; kind; range; children }

let symbols_of_result server ~path value =
  match value with
  | Jsont.Null _ -> Ok []
  | Jsont.Array (values, _) ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* symbol = symbol_of_json ~server ~path ~depth:0 value in
            collect (symbol :: acc) rest
      in
      collect [] values
  | _ -> Error (`Rpc (server.name, "invalid document symbols response"))

let parse_diagnostic ~path value =
  let* range =
    match member "range" value with
    | None -> Error "diagnostic has no range"
    | Some range -> range_json range
  in
  let* message =
    match string_member "message" value with
    | Some text -> Ok text
    | None -> Error "diagnostic has no message"
  in
  let source = string_member "source" value in
  let severity = diagnostic_severity (int_member "severity" value) in
  let line, col, _, _ = range in
  Ok { path; line = line + 1; col = col + 1; severity; message; source }

let update_diagnostics t value =
  match member "params" value with
  | Some params -> (
      match string_member "uri" params with
      | None -> ()
      | Some uri ->
          let path = normalize_path (path_of_uri uri) in
          let values =
            match member "diagnostics" params with
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
          Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
              let state =
                match Hashtbl.find_opt t.diagnostics_table path with
                | Some state -> state
                | None ->
                    let state = { values = []; serial = 0 } in
                    Hashtbl.add t.diagnostics_table path state;
                    state
              in
              state.values <- values;
              state.serial <- state.serial + 1))
  | None -> ()

let send_response server ~id value =
  ignore
    (send_json server
       (json_object [ ("jsonrpc", json_string "2.0"); ("id", id); ("result", value) ]))

let send_error_response server ~id code message =
  ignore
    (send_json server
       (json_object
          [
            ("jsonrpc", json_string "2.0");
            ("id", id);
            ( "error",
              json_object [ ("code", json_int code); ("message", json_string message) ] );
          ]))

let handle_response t server value =
  match response_id value with
  | None -> fail_server t server "response has no request id"
  | Some id ->
      let reply =
        match member "error" value with
        | Some error ->
            let code = Option.value ~default:(-32000) (int_member "code" error) in
            let message =
              Option.value ~default:"unknown LSP error" (string_member "message" error)
            in
            `Error (code, message)
        | None -> (
            match member "result" value with
            | Some result -> `Result result
            | None -> `Error (-32603, "response has no result"))
      in
      Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
          match Hashtbl.find_opt server.pending id with
          | None -> ()
          | Some pending ->
              pending.reply <- Some reply;
              Eio.Condition.broadcast pending.condition)

let handle_server_request server ~id ~method_ =
  if method_ = "workspace/configuration" then send_response server ~id (json_array [])
  else send_error_response server ~id (-32601) (Fmt.str "method not found: %s" method_)

let handle_message t server value =
  match object_value value with
  | None -> fail_server t server "JSON-RPC message is not an object"
  | Some _ -> (
      match string_member "method" value with
      | Some method_ -> (
          match member "id" value with
          | Some id -> handle_server_request server ~id ~method_
          | None ->
              if method_ = "textDocument/publishDiagnostics" then
                update_diagnostics t value
              else if method_ = "window/showMessage" then ()
              else ())
      | None -> handle_response t server value)

let reader_loop t server generation source =
  let fail message =
    if server.generation = generation then fail_server t server message
  in
  let reader = Eio.Buf_read.of_flow source ~max_size:max_frame_size in
  let rec loop () =
    match read_frame reader with
    | Error message -> fail message
    | Ok value ->
        handle_message t server value;
        loop ()
  in
  try loop () with
  | End_of_file -> fail "server EOF"
  | Eio.Buf_read.Buffer_limit_exceeded -> fail "server frame exceeds buffer limit"
  | Eio.Io (_, _) as exception_ -> fail (Fmt.str "%a" Eio.Exn.pp exception_)
  | Unix.Unix_error (error, function_name, argument) ->
      fail (Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument)
  | Failure message -> fail message
  | Eio.Cancel.Cancelled _ -> fail "server reader cancelled"

let stderr_loop source =
  let buffer = Buffer.create 256 in
  let captured = ref 0 in
  let chunk = Cstruct.create 65536 in
  let rec loop () =
    match Eio.Flow.single_read source chunk with
    | count when count > 0 ->
        let remaining = max_stderr_size - !captured in
        let keep = min remaining count in
        if keep > 0 then
          Buffer.add_substring buffer
            (Cstruct.to_string (Cstruct.sub chunk 0 keep))
            0 keep;
        captured := min max_stderr_size (!captured + count);
        loop ()
    | _ -> loop ()
  in
  try loop () with
  | End_of_file -> ()
  | Eio.Io _ -> ()
  | Unix.Unix_error _ -> ()
  | Eio.Cancel.Cancelled _ -> ()

let await_process t server generation process =
  Eio.Mutex.use_rw ~protect:true server.process_wait_lock (fun () ->
      match server.exit_status with
      | Some status -> status
      | None ->
          let status = Eio.Process.await process in
          Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
              if server.generation = generation then server.exit_status <- Some status);
          status)

let monitor_process t server generation process =
  try
    ignore (await_process t server generation process);
    if server.generation = generation && not server.stopping then
      fail_server t server "server exited"
  with
  | Eio.Io (_, _) as exception_ ->
      if server.generation = generation && not server.stopping then
        fail_server t server (Fmt.str "%a" Eio.Exn.pp exception_)
  | Unix.Unix_error (error, function_name, argument) ->
      if server.generation = generation && not server.stopping then
        fail_server t server
          (Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument)
  | Eio.Cancel.Cancelled _ -> ()

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
        Option.value ~default:(json_null ()) server.config.init_options );
    ]

let cleanup_streams server =
  Eio.Mutex.use_rw ~protect:true server.write_lock (fun () ->
      Option.iter Eio.Flow.close server.stdin;
      Option.iter Eio.Flow.close server.stdout;
      Option.iter Eio.Flow.close server.stderr;
      server.stdin <- None;
      server.stdout <- None;
      server.stderr <- None)

let start_server t server =
  try
    let stdin_source, stdin_sink = Eio.Process.pipe ~sw:t.sw t.proc_mgr in
    let stdout_source, stdout_sink = Eio.Process.pipe ~sw:t.sw t.proc_mgr in
    let stderr_source, stderr_sink = Eio.Process.pipe ~sw:t.sw t.proc_mgr in
    let argv = server.config.command :: server.config.args in
    let process_result =
      try
        Ok
          (Eio.Process.spawn ~sw:t.sw t.proc_mgr
             ~cwd:Eio.Path.(t.fs / t.cwd)
             ~stdin:stdin_source ~stdout:stdout_sink ~stderr:stderr_sink argv)
      with
      | Eio.Io (_, _) as exception_ -> Error (Fmt.str "%a" Eio.Exn.pp exception_)
      | Unix.Unix_error (error, function_name, argument) ->
          Error (Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument)
      | Failure message -> Error message
    in
    Eio.Flow.close stdin_source;
    Eio.Flow.close stdout_sink;
    Eio.Flow.close stderr_sink;
    match process_result with
    | Error message ->
        Eio.Flow.close stdin_sink;
        Eio.Flow.close stdout_source;
        Eio.Flow.close stderr_source;
        set_state t server (Failed message)
    | Ok process -> (
        let generation =
          Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
              server.process <- Some process;
              server.stdin <- Some stdin_sink;
              server.stdout <- Some stdout_source;
              server.stderr <- Some stderr_source;
              server.exit_status <- None;
              server.generation <- server.generation + 1;
              server.stopping <- false;
              server.generation)
        in
        Eio.Fiber.fork ~sw:t.sw (fun () -> reader_loop t server generation stdout_source);
        Eio.Fiber.fork ~sw:t.sw (fun () -> stderr_loop stderr_source);
        Eio.Fiber.fork ~sw:t.sw (fun () -> monitor_process t server generation process);
        match
          request t server ~timeout:30. ~method_:"initialize"
            ~params:(initialize_params t server)
        with
        | Error error ->
            let message = Fmt.str "%a" pp_error error in
            fail_server t server message
        | Ok _ -> (
            match notification server ~method_:"initialized" ~params:(json_object []) with
            | Error message -> fail_server t server message
            | Ok () -> set_state t server Ready))
  with
  | Eio.Cancel.Cancelled _ as exc ->
      Eio.Cancel.protect (fun () ->
          set_state t server (Failed "server startup cancelled"));
      raise exc
  | Eio.Io (_, _) as exception_ ->
      set_state t server (Failed (Fmt.str "%a" Eio.Exn.pp exception_))
  | Unix.Unix_error (error, function_name, argument) ->
      set_state t server
        (Failed (Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument))
  | Failure message -> set_state t server (Failed message)

let wait_started t server =
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      let rec loop () =
        match server.state with
        | Ready -> Ok ()
        | Failed message -> Error (`Not_ready message)
        | Disabled -> Error (`Not_ready server.name)
        | Not_started -> Error (`Not_ready server.name)
        | Starting ->
            Eio.Condition.await server.state_condition t.lock;
            loop ()
      in
      loop ())

let ensure_started t server =
  let start =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        match server.state with
        | Not_started ->
            server.state <- Starting;
            true
        | Starting | Ready | Failed _ | Disabled -> false)
  in
  if start then start_server t server;
  if start then
    match server.state with
    | Ready -> Ok ()
    | Failed message -> Error (`Not_ready message)
    | _ -> Error (`Not_ready server.name)
  else
    Eio.Time.with_timeout t.clock 30. (fun () -> wait_started t server) |> function
    | Ok () -> Ok ()
    | Error (`Not_ready message) -> Error (`Not_ready message)
    | Error `Timeout -> Error (`Not_ready server.name)

let send_document t server path text =
  let ext = extension path in
  let document =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
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
            document)
  in
  let result =
    if document.opened then
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
             ])
  in
  match result with
  | Ok () -> document.opened <- true
  | Error message -> fail_server t server message

let touch t ~path =
  let path = normalize_path path in
  match server_for_path t path with
  | None -> ()
  | Some server when not (has_root_marker t server) -> ()
  | Some server -> (
      let text =
        try Some (Eio.Path.load Eio.Path.(t.fs / path)) with
        | Eio.Io _ -> None
        | Unix.Unix_error _ -> None
      in
      match text with
      | None -> ()
      | Some text -> (
          match ensure_started t server with
          | Ok () -> send_document t server path text
          | Error _ -> ()))

let servers t =
  Eio.Mutex.use_ro t.lock (fun () ->
      List.map (fun server -> (server.name, server.state)) t.servers_table)

let handles t ~path =
  match server_for_path t (normalize_path path) with
  | Some server -> Some server.name
  | None -> None

let diagnostic_snapshot t path =
  Eio.Mutex.use_ro t.lock (fun () ->
      match Hashtbl.find_opt t.diagnostics_table path with
      | None -> ([], 0)
      | Some state -> (state.values, state.serial))

let diagnostics t ~path ~wait =
  let path = normalize_path path in
  let initial_values, initial_serial = diagnostic_snapshot t path in
  let deadline = Eio.Time.now t.clock +. max 0. (min 1. wait) in
  let first_publication = ref None in
  let serial = ref initial_serial in
  let rec loop () =
    let values, current_serial = diagnostic_snapshot t path in
    if current_serial <> !serial then (
      serial := current_serial;
      first_publication := Some (Eio.Time.now t.clock));
    let now = Eio.Time.now t.clock in
    if now >= deadline then values
    else
      match !first_publication with
      | Some published when now -. published >= 0.3 -> values
      | _ ->
          Eio.Time.sleep t.clock (min 0.05 (max 0.001 (deadline -. now)));
          loop ()
  in
  if wait <= 0. then initial_values else loop ()

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
  let path = normalize_path path in
  match server_for_path t path with
  | None -> Error (`No_server path)
  | Some server when not (has_root_marker t server) -> Error (`No_server path)
  | Some server ->
      touch t ~path;
      let* () = ensure_started t server in
      Ok (server, path)

let definition t ~path ~line ~col =
  if not (valid_position line col) then Error (`Rpc ("client", "invalid position"))
  else
    let* server, path = prepare_request t path in
    let* value =
      request t server ~timeout:10. ~method_:"textDocument/definition"
        ~params:(position_params ~path ~line ~col)
    in
    locations_of_result server value

let references t ~path ~line ~col =
  if not (valid_position line col) then Error (`Rpc ("client", "invalid position"))
  else
    let* server, path = prepare_request t path in
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
    let* value =
      request t server ~timeout:10. ~method_:"textDocument/references" ~params
    in
    locations_of_result server value

let document_symbols t ~path =
  let* server, path = prepare_request t path in
  let params =
    json_object
      [ ("textDocument", json_object [ ("uri", json_string (uri_of_path path)) ]) ]
  in
  let* value =
    request t server ~timeout:10. ~method_:"textDocument/documentSymbol" ~params
  in
  symbols_of_result server ~path value

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
  let* symbols = document_symbols t ~path in
  match find_exact name symbols with
  | Some symbol -> Ok (Some symbol)
  | None -> Ok (find_suffix name symbols)

let current_version server path =
  match Hashtbl.find_opt server.documents path with
  | Some document -> Some document.version
  | None -> None

let parse_text_edit server value =
  let* range =
    match member "range" value with
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
    match string_member "newText" value with
    | Some text when String.is_valid_utf_8 text -> Ok text
    | Some _ -> Error (`Rpc (server.name, "text edit is not valid UTF-8"))
    | None -> Error (`Rpc (server.name, "text edit has no newText"))
  in
  Ok { range; new_text }

let parse_file_edits server path value =
  match array_value value with
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

let parse_workspace_edit server value =
  let groups = ref [] in
  let parse_changes changes =
    match object_value changes with
    | None -> Error (`Rpc (server.name, "workspace changes is not an object"))
    | Some members ->
        let rec loop = function
          | [] -> Ok ()
          | ((name, _), edits) :: rest ->
              let path = normalize_path (path_of_uri name) in
              let* values = parse_file_edits server path edits in
              groups := merge_group path values !groups;
              loop rest
        in
        loop members
  in
  let parse_document_changes changes =
    match array_value changes with
    | None -> Error (`Rpc (server.name, "documentChanges is not an array"))
    | Some values ->
        let rec loop = function
          | [] -> Ok ()
          | value :: rest ->
              let* document =
                match member "textDocument" value with
                | Some document -> Ok document
                | None ->
                    Error (`Rpc (server.name, "resource operations are not supported"))
              in
              let* uri =
                match string_member "uri" document with
                | Some uri -> Ok uri
                | None -> Error (`Rpc (server.name, "document change has no URI"))
              in
              let path = normalize_path (path_of_uri uri) in
              let* () =
                match int_member "version" document with
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
                match member "edits" value with
                | Some edits -> parse_file_edits server path edits
                | None -> Error (`Rpc (server.name, "document change has no edits"))
              in
              groups := merge_group path edits !groups;
              loop rest
        in
        loop values
  in
  let* () =
    match member "changes" value with
    | Some changes -> parse_changes changes
    | None -> Ok ()
  in
  let* () =
    match member "documentChanges" value with
    | Some changes -> parse_document_changes changes
    | None -> Ok ()
  in
  Ok !groups

let rename t ~path ~line ~col ~new_name =
  if not (valid_position line col) then Error (`Rpc ("client", "invalid position"))
  else if new_name = "" then Error (`Rpc ("client", "new name is empty"))
  else
    let* server, path = prepare_request t path in
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
    let* value = request t server ~timeout:10. ~method_:"textDocument/rename" ~params in
    match value with Jsont.Null _ -> Ok [] | _ -> parse_workspace_edit server value

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

let apply_file_edits fs path edits =
  let path = normalize_path (path_of_uri path) in
  let original =
    try Ok (Eio.Path.load Eio.Path.(fs / path)) with
    | Eio.Io (_, _) as exception_ ->
        Error (`Io (path, Fmt.str "%a" Eio.Exn.pp exception_))
    | Unix.Unix_error (error, function_name, argument) ->
        Error
          (`Io
             (path, Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument))
  in
  let* original = original in
  if not (String.is_valid_utf_8 original) then
    Error (`Io (path, "source is not valid UTF-8"))
  else
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
    let replacement = Buffer.contents buffer in
    try
      Eio.Cancel.protect (fun () ->
          Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) replacement);
      Ok path
    with
    | Eio.Io (_, _) as exception_ ->
        Error (`Io (path, Fmt.str "%a" Eio.Exn.pp exception_))
    | Unix.Unix_error (error, function_name, argument) ->
        Error
          (`Io
             (path, Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument))
    | Eio.Cancel.Cancelled _ as exc -> raise exc

let apply_edits ~fs grouped =
  let rec apply acc = function
    | [] -> Ok (List.rev acc)
    | (path, edits) :: rest ->
        let* touched = apply_file_edits fs path edits in
        apply (touched :: acc) rest
  in
  apply [] grouped

let reopen_documents t server =
  let documents =
    Eio.Mutex.use_ro t.lock (fun () ->
        Hashtbl.fold
          (fun path document acc -> (path, document.text) :: acc)
          server.documents [])
  in
  List.iter (fun (path, text) -> send_document t server path text) documents

let stop_server t server ~final =
  server.stopping <- true;
  let process = server.process in
  let generation = server.generation in
  (match server.state with
  | Ready -> (
      (match request t server ~timeout:2. ~method_:"shutdown" ~params:(json_null ()) with
      | Ok _ | Error _ -> ());
      match notification server ~method_:"exit" ~params:(json_null ()) with
      | Ok () | Error _ -> ())
  | _ -> ());
  Option.iter (fun process -> Eio.Process.signal process Sys.sigterm) process;
  Option.iter
    (fun process ->
      let awaited =
        try
          Eio.Time.with_timeout t.clock 2. (fun () ->
              Ok (await_process t server generation process))
        with Eio.Cancel.Cancelled _ -> Error `Timeout
      in
      match awaited with
      | Ok _ -> ()
      | Error `Timeout -> Eio.Process.signal process Sys.sigkill)
    process;
  Eio.Cancel.protect (fun () -> cleanup_streams server);
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      server.process <- None;
      Hashtbl.iter
        (fun _ document ->
          document.opened <- false;
          document.version <- 0)
        server.documents;
      mark_pending server (`Error (-32000, "server stopped"));
      if final then server.state <- Disabled else server.state <- Not_started;
      Eio.Condition.broadcast server.state_condition)

let restart t ~name =
  let selected =
    match name with
    | None -> Ok t.servers_table
    | Some name -> (
        match find_server_by_name t name with
        | Some server -> Ok [ server ]
        | None -> Error (`No_server name))
  in
  let* selected = selected in
  let restarted = ref [] and failed = ref [] in
  List.iter
    (fun server ->
      stop_server t server ~final:false;
      if not (has_root_marker t server) then failed := server.name :: !failed
      else (
        ignore (ensure_started t server);
        match server.state with
        | Ready ->
            restarted := server.name :: !restarted;
            reopen_documents t server
        | Failed _ -> failed := server.name :: !failed
        | _ -> failed := server.name :: !failed))
    selected;
  Ok (List.rev !restarted, List.rev !failed)

let stop_all t =
  List.iter (fun server -> stop_server t server ~final:true) t.servers_table

let create ~sw ~proc_mgr ~clock ~fs ~cwd ~config =
  let cwd = normalize_path cwd in
  let configured =
    let defaults_by_name = defaults in
    let base = if config.Config.options.auto_lsp then defaults_by_name else [] in
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
          match List.assoc_opt name defaults_by_name with
          | Some default when value.filetypes = [] ->
              {
                value with
                filetypes = default.filetypes;
                root_markers = default.root_markers;
              }
          | _ -> value
        in
        replace name value values)
      base config.Config.lsp
  in
  let lock = Eio.Mutex.create () in
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
          process_wait_lock = Eio.Mutex.create ();
          generation = 0;
          stopping = false;
          next_id = 1;
          pending = Hashtbl.create 32;
          documents = Hashtbl.create 32;
          state_condition = Eio.Condition.create ();
          write_lock = Eio.Mutex.create ();
        })
      configured
  in
  let t =
    {
      sw;
      proc_mgr;
      clock;
      fs;
      cwd;
      servers_table;
      lock;
      diagnostics_table = Hashtbl.create 64;
    }
  in
  Eio.Switch.on_release sw (fun () -> Eio.Cancel.protect (fun () -> stop_all t));
  t
