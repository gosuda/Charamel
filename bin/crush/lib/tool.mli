(** Crush tool contracts and execution context.

    Tools decode provider JSON, authorize the decoded operation, and return bounded,
    model-visible output. Filesystem helpers preserve a canonical path boundary for
    callers that perform I/O. *)

type diagnostic = Lsp.diagnostic
(** The type for diagnostics attached to a tool result. *)

type question = {
  header : string;
  text : string;
  options : (string * string) list;
  multi : bool;
  free_text : bool;
}
(** The type for one interactive question offered by a tool. *)

type answer = { header : string; selected : string list; text : string option }
(** The type for one answer returned by the interactive question boundary. *)

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
(** The type for capabilities and per-call state supplied to a tool. *)

type output = {
  content : string;
  is_error : bool;
  artifact : string option;
  diagnostics : diagnostic list;
}
(** The type for output returned by a tool. *)

val ok : ?artifact:string -> ?diagnostics:diagnostic list -> string -> output
(** [ok ?artifact ?diagnostics content] is a successful output containing [content].
    [artifact] defaults to [None]. [diagnostics] defaults to the empty list. *)

val fail : string -> output
(** [fail content] is an error output containing [content]. *)

val to_result : output -> [ `Text of string | `Error of string ]
(** [to_result output] is the provider-facing result represented by [output]. Diagnostics
    are appended to the content in a stable, human-readable form. *)

type error =
  [ `Invalid_input of string
  | `Denied of string
  | `Not_found of string
  | `Io of string * string
  | `Timeout of float
  | `Unavailable of string
  | `Aborted ]
(** The type for recoverable tool failures. *)

val pp_error : error Fmt.t
(** [pp_error ppf error] formats [error] with its operation context. *)

type t = {
  name : string;
  description : string;
  schema : Jsont.json;
  read_only : bool;
  run : ctx -> Jsont.json -> (output, error) result;
}
(** The type for one model-callable tool. [read_only] is conservative scheduling metadata
    and is true only when every invocation is read-only. *)

val to_fantasy : t -> Charamel_fantasy.Tool.t
(** [to_fantasy tool] is the provider-neutral definition of [tool]. *)

val decode : 'a Jsont.t -> Jsont.json -> ('a, error) result
(** [decode codec value] decodes [value] with [codec], or reports an invalid model input
    with the codec diagnostic. *)

val absolute : ctx -> string -> string
(** [absolute ctx path] resolves [path] against [ctx.cwd]. A leading [~/] uses the home
    directory from [ctx.env]. Dot and dot-dot components are normalized lexically. *)

val within_cwd : ctx -> string -> bool
(** [within_cwd ctx path] is [true] when [path] is lexically contained in the normalized
    project directory [ctx.cwd]. Component boundaries are respected. *)

val canonical : ctx -> string -> (string, error) result
(** [canonical ctx path] resolves [path] to an existing canonical filesystem path.
    Filesystem failures retain the path and operation context. *)

val canonical_parent : ctx -> string -> (string, error) result
(** [canonical_parent ctx path] resolves the existing parent of [path] and appends its
    final component. It is the path check for a new file. *)

val request :
  ctx ->
  read_only:bool ->
  tool:string ->
  action:string ->
  path:string ->
  description:string ->
  (unit, error) result
(** [request ctx ~read_only ~tool ~action ~path ~description] authorizes one decoded
    operation through [ctx.permission]. [read_only] is supplied by trusted tool code
    rather than model JSON. *)

val with_timeout : ctx -> float -> (unit -> 'a) -> ('a, error) result
(** [with_timeout ctx seconds f] runs [f] for at most [seconds] seconds using [ctx.clock].
    A deadline expiry is returned as [`Timeout seconds]. *)

val schema_object : ?required:string list -> (string * Jsont.json) list -> Jsont.json
(** [schema_object ?required fields] is a JSON object schema with [fields] as properties.
    [required] defaults to the empty list. *)

val s_string : ?desc:string -> ?enum:string list -> unit -> Jsont.json
(** [s_string ?desc ?enum ()] is a JSON string schema. [desc] and [enum] default to
    absent. *)

val s_int : ?desc:string -> ?default:int -> unit -> Jsont.json
(** [s_int ?desc ?default ()] is a JSON integer schema. [desc] and [default] default to
    absent. *)

val s_bool : ?desc:string -> ?default:bool -> unit -> Jsont.json
(** [s_bool ?desc ?default ()] is a JSON boolean schema. [desc] and [default] default to
    absent. *)

val s_array : ?desc:string -> Jsont.json -> Jsont.json
(** [s_array ?desc item] is a JSON array schema whose items use [item]. *)

val s_object :
  ?desc:string -> ?required:string list -> (string * Jsont.json) list -> Jsont.json
(** [s_object ?desc ?required fields] is a nested JSON object schema. *)
