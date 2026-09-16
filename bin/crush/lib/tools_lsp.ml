let ( let* ) = Result.bind

type path_args = { path : string option }
type symbol_args = { symbol : string; path : string option }
type rename_args = { symbol : string; new_name : string; path : string option }
type restart_args = { name : string option }

let path_codec =
  let open Jsont in
  Object.map (fun (path : string option) -> ({ path } : path_args))
  |> Object.mem "path" (option string) ~dec_absent:None
       ~enc:(fun (value : path_args) -> value.path)
       ~enc_omit:Option.is_none
  |> Object.finish

let symbol_codec =
  let open Jsont in
  Object.map (fun (symbol : string) (path : string option) ->
      ({ symbol; path } : symbol_args))
  |> Object.mem "symbol" string ~enc:(fun (value : symbol_args) -> value.symbol)
  |> Object.mem "path" (option string) ~dec_absent:None
       ~enc:(fun (value : symbol_args) -> value.path)
       ~enc_omit:Option.is_none
  |> Object.finish

let rename_codec =
  let open Jsont in
  Object.map (fun (symbol : string) (new_name : string) (path : string option) ->
      ({ symbol; new_name; path } : rename_args))
  |> Object.mem "symbol" string ~enc:(fun (value : rename_args) -> value.symbol)
  |> Object.mem "new_name" string ~enc:(fun (value : rename_args) -> value.new_name)
  |> Object.mem "path" (option string) ~dec_absent:None
       ~enc:(fun (value : rename_args) -> value.path)
       ~enc_omit:Option.is_none
  |> Object.finish

let symbols_codec =
  let open Jsont in
  Object.map (fun (path : string) -> ({ path = Some path } : path_args))
  |> Object.mem "path" string ~enc:(fun (value : path_args) ->
      Option.value value.path ~default:"")
  |> Object.finish

let restart_codec =
  let open Jsont in
  Object.map (fun (name : string option) -> ({ name } : restart_args))
  |> Object.mem "name" (option string) ~dec_absent:None
       ~enc:(fun (value : restart_args) -> value.name)
       ~enc_omit:Option.is_none
  |> Object.finish

let path_schema ?(description = "File path") () = Tool.s_string ~desc:description ()
let diagnostics_schema = Tool.schema_object [ ("path", path_schema ()) ]

let navigation_schema =
  Tool.schema_object ~required:[ "symbol" ]
    [ ("symbol", Tool.s_string ~desc:"Symbol name" ()); ("path", path_schema ()) ]

let rename_schema =
  Tool.schema_object ~required:[ "symbol"; "new_name" ]
    [
      ("symbol", Tool.s_string ~desc:"Symbol name" ());
      ("new_name", Tool.s_string ~desc:"Replacement symbol name" ());
      ("path", path_schema ());
    ]

let restart_schema = Tool.schema_object [ ("name", Tool.s_string ~desc:"Server name" ()) ]

let map_lsp_error = function
  | `No_server path -> `Unavailable (Fmt.str "no LSP server for %s" path)
  | `Not_ready server -> `Unavailable (Fmt.str "LSP server %s is not ready" server)
  | `Rpc (server, message) -> `Unavailable (Fmt.str "LSP %s: %s" server message)
  | `Timeout _ -> `Timeout 10.
  | `Io (path, message) -> `Io (path, message)

let canonical_or_absolute ctx path =
  let absolute = Tool.absolute ctx path in
  match Tool.canonical ctx absolute with Ok canonical -> canonical | Error _ -> absolute

let canonical_path (ctx : Tool.ctx) = function
  | None | Some "" -> ctx.cwd
  | Some path -> canonical_or_absolute ctx path

let requested_paths (ctx : Tool.ctx) = function
  | Some path when path <> "" -> [ canonical_or_absolute ctx path ]
  | _ ->
      let paths = Hashtbl.fold (fun path _ acc -> path :: acc) ctx.read_tracker [] in
      if paths = [] then [] else List.sort String.compare paths

let path_for_permission (ctx : Tool.ctx) = function
  | Some path when path <> "" -> canonical_or_absolute ctx path
  | _ -> ctx.cwd

let with_lsp (ctx : Tool.ctx) f =
  match ctx.lsp with
  | None -> Error (`Unavailable "no LSP configured")
  | Some lsp -> f lsp

let touch_file lsp path = if path <> "" && path <> "/" then Lsp.touch lsp ~path

let truncate_output (ctx : Tool.ctx) ?diagnostics text =
  let content, artifact = Artifact.truncate ctx.artifacts ~random:ctx.random text in
  Tool.ok ?artifact ?diagnostics content

let severity = function
  | `Error -> "error"
  | `Warning -> "warning"
  | `Info -> "info"
  | `Hint -> "hint"

let format_diagnostic (diagnostic : Lsp.diagnostic) =
  Fmt.str "%s:%d:%d %s %s" diagnostic.path diagnostic.line diagnostic.col
    (severity diagnostic.severity)
    diagnostic.message

let diagnostics_text diagnostics =
  match diagnostics with
  | [] -> "no diagnostics"
  | values -> String.concat "\n" (List.map format_diagnostic values)

let run_diagnostics ctx input =
  let* ({ path } : path_args) = Tool.decode path_codec input in
  let permission_path = path_for_permission ctx path in
  let* () =
    Tool.request ctx ~read_only:true ~tool:"lsp_diagnostics" ~action:"diagnostics"
      ~path:permission_path ~description:"Read language-server diagnostics"
  in
  with_lsp ctx (fun lsp ->
      let paths = requested_paths ctx path in
      let outcome =
        Tool.with_timeout ctx 2. (fun () ->
            List.concat_map
              (fun path ->
                touch_file lsp path;
                Lsp.diagnostics lsp ~path ~wait:0.5)
              paths)
      in
      match outcome with
      | Error error -> Error error
      | Ok diagnostics -> Ok (truncate_output ctx (diagnostics_text diagnostics)))

let file_uri_path path =
  if String.starts_with ~prefix:"file://" path then
    let start = 7 in
    String.sub path start (String.length path - start)
  else path

let read_context (ctx : Tool.ctx) (location : Lsp.location) =
  let path = file_uri_path location.path in
  let first = max 1 (location.line - 5) in
  let last = max first (location.end_line + 5) in
  try
    let lines = ref [] in
    let line_number = ref 1 in
    Eio.Path.with_lines
      Eio.Path.(ctx.fs / path)
      (fun stream ->
        let rec consume stream =
          if !line_number > last then ()
          else
            match stream () with
            | Seq.Nil -> ()
            | Seq.Cons (line, next) ->
                if !line_number >= first then
                  lines := Fmt.str "%d:%s" !line_number line :: !lines;
                incr line_number;
                consume next
        in
        consume stream);
    List.rev !lines
  with Eio.Io _ -> []

let format_location ctx (location : Lsp.location) =
  let path = file_uri_path location.path in
  let header =
    Fmt.str "%s:%d:%d-%d:%d" path location.line location.col location.end_line
      location.end_col
  in
  match read_context ctx location with
  | [] -> header
  | context ->
      header ^ "\n" ^ String.concat "\n" (List.map (fun line -> "  " ^ line) context)

let locations_text ctx locations =
  match locations with
  | [] -> "no locations"
  | values -> String.concat "\n\n" (List.map (format_location ctx) values)

let locate_symbol (ctx : Tool.ctx) lsp path name =
  let source = canonical_path ctx path in
  if source <> ctx.cwd then touch_file lsp source;
  match Lsp.find_symbol lsp ~path:source ~name with
  | Ok (Some symbol) -> Ok symbol
  | Ok None -> Error (`Not_found name)
  | Error error -> Error (map_lsp_error error)

let run_definition ctx input =
  let* ({ symbol; path } : symbol_args) = Tool.decode symbol_codec input in
  let* () =
    Tool.request ctx ~read_only:true ~tool:"lsp_definition" ~action:"definition"
      ~path:(path_for_permission ctx path)
      ~description:"Find a symbol definition"
  in
  with_lsp ctx (fun lsp ->
      let* symbol = locate_symbol ctx lsp path symbol in
      let location = (symbol : Lsp.symbol).Lsp.range in
      match
        Lsp.definition lsp ~path:location.path ~line:location.line ~col:location.col
      with
      | Ok locations -> Ok (truncate_output ctx (locations_text ctx locations))
      | Error error -> Error (map_lsp_error error))

let run_references ctx input =
  let* ({ symbol; path } : symbol_args) = Tool.decode symbol_codec input in
  let* () =
    Tool.request ctx ~read_only:true ~tool:"lsp_references" ~action:"references"
      ~path:(path_for_permission ctx path)
      ~description:"Find symbol references"
  in
  with_lsp ctx (fun lsp ->
      let* found = locate_symbol ctx lsp path symbol in
      let location = (found : Lsp.symbol).Lsp.range in
      match
        Lsp.references lsp ~path:location.path ~line:location.line ~col:location.col
      with
      | Ok locations ->
          let limited =
            if List.length locations > 200 then
              List.filteri (fun index _ -> index < 200) locations
            else locations
          in
          Ok (truncate_output ctx (locations_text ctx limited))
      | Error error -> Error (map_lsp_error error))

let rec symbol_lines indent (symbol : Lsp.symbol) =
  let line =
    Fmt.str "%s%s %s (%d)" (String.make indent ' ') symbol.kind symbol.name
      symbol.range.line
  in
  line :: List.concat_map (symbol_lines (indent + 2)) symbol.children

let run_symbols ctx input =
  let* ({ path } : path_args) = Tool.decode symbols_codec input in
  let* path =
    match path with
    | Some value when String.trim value <> "" -> Ok (Some value)
    | _ -> Error (`Invalid_input "path must not be empty")
  in
  let source = canonical_path ctx path in
  let* () =
    Tool.request ctx ~read_only:true ~tool:"lsp_symbols" ~action:"symbols" ~path:source
      ~description:"List document symbols"
  in
  with_lsp ctx (fun lsp ->
      touch_file lsp source;
      match Lsp.document_symbols lsp ~path:source with
      | Ok symbols ->
          let lines = List.concat_map (symbol_lines 0) symbols in
          Ok
            (truncate_output ctx
               (if lines = [] then "no symbols" else String.concat "\n" lines))
      | Error error -> Error (map_lsp_error error))

let run_rename (ctx : Tool.ctx) input =
  let* ({ symbol; new_name; path } : rename_args) = Tool.decode rename_codec input in
  let source = canonical_path ctx path in
  let permission_path = if Permission.plan_mode ctx.permission then "" else source in
  let* () =
    Tool.request ctx ~read_only:false ~tool:"lsp_rename" ~action:"rename"
      ~path:permission_path
      ~description:(Fmt.str "Rename %s to %s" symbol new_name)
  in
  with_lsp ctx (fun lsp ->
      touch_file lsp source;
      let* found = locate_symbol ctx lsp path symbol in
      let location = (found : Lsp.symbol).Lsp.range in
      match
        Lsp.rename lsp ~path:location.path ~line:location.line ~col:location.col ~new_name
      with
      | Error error -> Error (map_lsp_error error)
      | Ok edits -> (
          match Lsp.apply_edits ~fs:ctx.fs edits with
          | Error error -> Error (map_lsp_error error)
          | Ok touched ->
              let diagnostics =
                match touched with
                | first :: _ -> Lsp.diagnostics lsp ~path:first ~wait:0.5
                | [] -> []
              in
              let files =
                touched |> List.map (fun path -> "  " ^ path) |> String.concat "\n"
              in
              let text =
                Fmt.str "renamed %s -> %s in %d files%s" symbol new_name
                  (List.length touched)
                  (if files = "" then "" else "\n" ^ files)
              in
              Ok (truncate_output ctx ~diagnostics text)))

let run_restart ctx input =
  let* ({ name } : restart_args) = Tool.decode restart_codec input in
  let* () =
    Tool.request ctx ~read_only:false ~tool:"lsp_restart" ~action:"restart" ~path:""
      ~description:"Restart language servers"
  in
  with_lsp ctx (fun lsp ->
      match Lsp.restart lsp ~name with
      | Error error -> Error (map_lsp_error error)
      | Ok (restarted, failed) ->
          let group values = if values = [] then "none" else String.concat ", " values in
          Ok
            (truncate_output ctx
               (Fmt.str "restarted: %s; failed: %s" (group restarted) (group failed))))

let lsp_diagnostics =
  {
    Tool.name = "lsp_diagnostics";
    description = "Read diagnostics from the configured language server";
    schema = diagnostics_schema;
    read_only = true;
    run = run_diagnostics;
  }

let lsp_definition =
  {
    Tool.name = "lsp_definition";
    description = "Find the definition of a symbol through LSP";
    schema = navigation_schema;
    read_only = true;
    run = run_definition;
  }

let lsp_references =
  {
    Tool.name = "lsp_references";
    description = "Find references to a symbol through LSP";
    schema = navigation_schema;
    read_only = true;
    run = run_references;
  }

let lsp_symbols =
  {
    Tool.name = "lsp_symbols";
    description = "List document symbols through LSP";
    schema = Tool.schema_object ~required:[ "path" ] [ ("path", path_schema ()) ];
    read_only = true;
    run = run_symbols;
  }

let lsp_rename =
  {
    Tool.name = "lsp_rename";
    description = "Rename a symbol across its language-server workspace";
    schema = rename_schema;
    read_only = false;
    run = run_rename;
  }

let lsp_restart =
  {
    Tool.name = "lsp_restart";
    description = "Restart one or all configured language servers";
    schema = restart_schema;
    read_only = false;
    run = run_restart;
  }

let all =
  [
    lsp_diagnostics; lsp_definition; lsp_references; lsp_symbols; lsp_rename; lsp_restart;
  ]
