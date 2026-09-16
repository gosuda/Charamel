type diagnostic = Lsp.diagnostic

type question = {
  header : string;
  text : string;
  options : (string * string) list;
  multi : bool;
  free_text : bool;
}

type answer = { header : string; selected : string list; text : string option }

type ctx = {
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  net : Eio_unix.Net.t;
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  random : int -> string;
  env : string -> string option;
  cwd : string;
  session : string;
  call_id : string;
  config : Config.t;
  permission : Permission.t;
  hooks : Hooks.t;
  lsp : Lsp.t option;
  mcp : Mcp.t;
  artifacts : Artifact.t;
  jobs : Jobs.t;
  todos : Todos.t;
  skills : Skills.t;
  log_path : string;
  interactive : bool;
  is_subagent : bool;
  ask : (question list -> (answer list, [ `Aborted | `Not_interactive ]) result) option;
  run_subagent : (prompt:string -> (string, string) result) option;
  read_tracker : (string, int) Hashtbl.t;
}

type output = {
  content : string;
  is_error : bool;
  artifact : string option;
  diagnostics : diagnostic list;
}

let ok ?artifact ?(diagnostics = []) content =
  { content; is_error = false; artifact; diagnostics }

let fail content = { content; is_error = true; artifact = None; diagnostics = [] }

let severity_name : [ `Error | `Warning | `Info | `Hint ] -> string = function
  | `Error -> "error"
  | `Warning -> "warning"
  | `Info -> "info"
  | `Hint -> "hint"

let diagnostic_text (diagnostic : diagnostic) =
  Fmt.str "%s:%d:%d %s %s" diagnostic.path diagnostic.line diagnostic.col
    (severity_name diagnostic.severity)
    diagnostic.message

let to_result output =
  let content =
    match output.diagnostics with
    | [] -> output.content
    | diagnostics ->
        output.content ^ "\n\nDiagnostics:\n"
        ^ String.concat "\n" (List.map diagnostic_text diagnostics)
  in
  if output.is_error then `Error content else `Text content

type error =
  [ `Invalid_input of string
  | `Denied of string
  | `Not_found of string
  | `Io of string * string
  | `Timeout of float
  | `Unavailable of string
  | `Aborted ]

let pp_error ppf = function
  | `Invalid_input message -> Fmt.pf ppf "invalid input: %s" message
  | `Denied message -> Fmt.pf ppf "permission denied: %s" message
  | `Not_found path -> Fmt.pf ppf "not found: %s" path
  | `Io (path, message) -> Fmt.pf ppf "I/O error for %s: %s" path message
  | `Timeout seconds -> Fmt.pf ppf "operation timed out after %.3g seconds" seconds
  | `Unavailable message -> Fmt.pf ppf "unavailable: %s" message
  | `Aborted -> Fmt.string ppf "aborted"

type t = {
  name : string;
  description : string;
  schema : Jsont.json;
  read_only : bool;
  run : ctx -> Jsont.json -> (output, error) result;
}

let to_fantasy { name; description; schema; _ } =
  Charm_fantasy.Tool.v ~name ~description ~schema

let decode codec value =
  match Jsont.Json.decode codec value with
  | Ok decoded -> Ok decoded
  | Error message -> Error (`Invalid_input message)

let split_components path =
  String.split_on_char '/' path |> List.filter (fun part -> part <> "")

let normalize_path path =
  let is_absolute = String.length path > 0 && path.[0] = '/' in
  let components = split_components path in
  let stack = ref [] in
  List.iter
    (fun component ->
      match component with
      | "." -> ()
      | ".." -> (
          match !stack with
          | top :: rest when top <> ".." -> stack := rest
          | _ when not is_absolute -> stack := ".." :: !stack
          | _ -> ())
      | value -> stack := value :: !stack)
    components;
  let body = String.concat "/" (List.rev !stack) in
  match (is_absolute, body = "") with
  | true, true -> "/"
  | true, false -> "/" ^ body
  | false, true -> "."
  | false, false -> body

let home_dir ctx =
  match ctx.env "HOME" with Some home when home <> "" -> home | _ -> ctx.cwd

let absolute ctx path =
  let path =
    if path = "~" then home_dir ctx
    else if String.length path >= 2 && String.sub path 0 2 = "~/" then
      Filename.concat (home_dir ctx) (String.sub path 2 (String.length path - 2))
    else path
  in
  let path =
    if String.length path > 0 && path.[0] = '/' then path
    else Filename.concat ctx.cwd path
  in
  normalize_path path

let component_prefix root path =
  let root = normalize_path root in
  let path = normalize_path path in
  root = "/" || path = root
  || String.length path > String.length root
     && String.starts_with ~prefix:(root ^ "/") path

let within_cwd ctx path = component_prefix ctx.cwd (absolute ctx path)

let unix_error path operation exception_ =
  match exception_ with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Error (`Not_found path)
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (`Io
           ( path,
             Fmt.str "%s: %s (%s %s)" operation (Unix.error_message error) function_name
               argument ))
  | Sys_error message -> Error (`Io (path, Fmt.str "%s: %s" operation message))
  | Invalid_argument message ->
      Error (`Invalid_input (Fmt.str "%s: %s" operation message))
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Error (`Not_found path)
  | Eio.Io (Eio.Fs.E _, _) as exception_ ->
      Error (`Io (path, Fmt.str "%s: %a" operation Eio.Exn.pp exception_))
  | Eio.Io _ as exception_ ->
      Error (`Io (path, Fmt.str "%s: %a" operation Eio.Exn.pp exception_))
  | exception_ -> Printexc.raise_with_backtrace exception_ (Printexc.get_raw_backtrace ())

let canonical ctx path =
  let path = absolute ctx path in
  try Ok (Eio_unix.run_in_systhread (fun () -> Unix.realpath path)) with
  | Unix.Unix_error _ as exception_ -> unix_error path "realpath" exception_
  | Sys_error _ as exception_ -> unix_error path "realpath" exception_
  | Invalid_argument _ as exception_ -> unix_error path "realpath" exception_
  | Eio.Io _ as exception_ -> unix_error path "realpath" exception_

let canonical_parent ctx path =
  let path = absolute ctx path in
  match canonical ctx path with
  | Ok resolved -> Ok resolved
  | Error (`Not_found _) -> (
      if path = "/" then Error (`Not_found path)
      else
        let parent = Filename.dirname path in
        let base = Filename.basename path in
        match canonical ctx parent with
        | Error _ as error -> error
        | Ok resolved_parent -> Ok (Filename.concat resolved_parent base))
  | Error error -> Error error

let request ctx ~read_only ~tool ~action ~path ~description =
  let request : Permission.request =
    { session = ctx.session; tool; action; path; description; read_only }
  in
  match Permission.resolve ctx.permission request with
  | Permission.Allowed -> Ok ()
  | Permission.Denied message -> Error (`Denied message)

let with_timeout ctx seconds f =
  if seconds < 0. then Error (`Invalid_input "timeout must not be negative")
  else
    match Eio.Time.with_timeout ctx.clock seconds (fun () -> Ok (f ())) with
    | Ok value -> Ok value
    | Error `Timeout -> Error (`Timeout seconds)

let json_member name value = Jsont.Json.mem (Jsont.Json.name name) value
let json_string value = Jsont.Json.string value
let json_bool value = Jsont.Json.bool value
let json_int value = Jsont.Json.int value

let schema_object ?(required = []) fields =
  let properties =
    Jsont.Json.object' (List.map (fun (name, schema) -> json_member name schema) fields)
  in
  let required_member =
    if required = [] then []
    else [ json_member "required" (Jsont.Json.list (List.map json_string required)) ]
  in
  Jsont.Json.object'
    ([
       json_member "type" (json_string "object");
       json_member "properties" properties;
       json_member "additionalProperties" (json_bool false);
     ]
    @ required_member)

let schema_scalar ?desc ?enum ?default kind =
  let description =
    match desc with
    | None -> []
    | Some value -> [ json_member "description" (json_string value) ]
  in
  let enumeration =
    match enum with
    | None -> []
    | Some values ->
        [ json_member "enum" (Jsont.Json.list (List.map json_string values)) ]
  in
  let default =
    match default with None -> [] | Some value -> [ json_member "default" value ]
  in
  Jsont.Json.object'
    ([ json_member "type" (json_string kind) ] @ description @ enumeration @ default)

let s_string ?desc ?enum () = schema_scalar ?desc ?enum "string"

let s_int ?desc ?default () =
  schema_scalar ?desc ?default:(Option.map json_int default) "integer"

let s_bool ?desc ?default () =
  schema_scalar ?desc ?default:(Option.map json_bool default) "boolean"

let s_array ?desc item =
  let description =
    match desc with
    | None -> []
    | Some value -> [ json_member "description" (json_string value) ]
  in
  Jsont.Json.object'
    ([ json_member "type" (json_string "array"); json_member "items" item ] @ description)

let s_object ?desc ?required fields =
  let schema = schema_object ?required fields in
  match desc with
  | None -> schema
  | Some value -> (
      match schema with
      | Jsont.Object (members, meta) ->
          Jsont.Object (json_member "description" (json_string value) :: members, meta)
      | _ -> schema)
