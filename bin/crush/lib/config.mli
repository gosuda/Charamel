(** Crush configuration.

    [Config] loads layered JSON configuration files, validates every supported section,
    expands environment references in command-facing strings, and derives project-scoped
    paths. *)

type provider_kind =
  | Anthropic
  | Openai
  | Openai_compatible
  | Openai_responses
  | Google  (** The provider wire kind. *)

type provider = {
  kind : provider_kind;
  base_url : string option;
  api_key : string option;
  headers : (string * string) list;
  models : Charm_fantasy.Model.t list;
}
(** A configured provider. Model entries are stamped with their provider id. *)

type reasoning = [ `Off | `Low | `Medium | `High ]
(** The reasoning effort requested from a model. *)

type selected_model = {
  provider : string;
  model : string;
  reasoning : reasoning option;
  max_tokens : int option;
}
(** A configured model selection. *)

type models = { large : selected_model option; small : selected_model option }
(** The large and small model selections. *)

type permissions = { allowed_tools : string list; deny : string list }
(** Permission entries. A member is a tool name or [tool:action-prefix]. *)

type mcp_transport = Stdio | Http  (** MCP transport kinds. *)

type mcp = {
  transport : mcp_transport;
  command : string option;
  args : string list;
  env : (string * string) list;
  url : string option;
  headers : (string * string) list;
  timeout_s : int;
}
(** One MCP server declaration. *)

type lsp = {
  command : string;
  args : string list;
  filetypes : string list;
  root_markers : string list;
  init_options : Jsont.json option;
}
(** One LSP server declaration. Filetypes do not include a leading dot. *)

type hook_event =
  | Pre_tool
  | Post_tool
  | Session_start
  | Stop  (** A lifecycle hook event. *)

type hook = {
  event : hook_event;
  matcher : string option;
  command : string;
  timeout_s : int;
}
(** One shell hook declaration. *)

type trailer = Trailer_none | Co_authored | Assisted  (** Attribution trailer mode. *)

type attribution = { trailer : trailer; generated_with : bool }
(** Generated-change attribution settings. *)

type advisor = { enabled : bool; model : [ `Small | `Large ]; every_n_turns : int }
(** Watchdog advisor settings. *)

type budgets = { subagent_requests : int }
(** Resource budgets. *)

type options = {
  data_dir : string;
  debug : bool;
  disable_auto_compaction : bool;
  auto_lsp : bool;
  attribution : attribution;
  advisor : advisor;
  budgets : budgets;
}
(** General runtime options. *)

type t = {
  providers : (string * provider) list;
  models : models;
  permissions : permissions;
  mcp : (string * mcp) list;
  lsp : (string * lsp) list;
  context_paths : string list;
  skills_paths : string list;
  hooks : hook list;
  options : options;
}
(** The complete validated configuration. *)

val default : t
(** [default] is the configuration used when no file contributes a value. *)

val jsont : t Jsont.t
(** [jsont] decodes and encodes configuration JSON. Every supported member is optional and
    defaults as documented by [default]. Unknown members of configuration records are
    rejected. Arbitrary keys remain valid in header, environment, and server-owned JSON
    maps. *)

type error = [ `Parse of string * string | `Io of string * string ]
(** A configuration loading error. The first component is the path. *)

val pp_error : error Fmt.t
(** [pp_error] formats a configuration error. *)

val expand_env : env:(string -> string option) -> string -> string
(** [expand_env ~env s] expands [$VAR], [${VAR}], and [$$] in [s]. Unset variables become
    the empty string. *)

val search_paths :
  cwd:string -> git_root:string option -> home_config:string -> string list
(** [search_paths ~cwd ~git_root ~home_config] returns the ordered, duplicate-free
    configuration candidates from the home directory through the project root to [cwd],
    each candidate ending in [crush.json]. *)

val merge : t -> t -> t
(** [merge left right] overlays [right] on [left]. Scalar members use right precedence;
    keyed declarations replace by key; list settings concatenate with stable
    order-preserving deduplication. *)

val load :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  env:(string -> string option) ->
  cwd:string ->
  (t * string list, error) result
(** [load ~fs ~env ~cwd] discovers and folds existing configuration files. *)

val schema : Jsont.json
(** [schema] is a JSON Schema 2020-12 description of [jsont]. *)

val data_dir : t -> cwd:string -> string
(** [data_dir config ~cwd] resolves [config.options.data_dir] against [cwd]. *)

val project_key : cwd:string -> string
(** [project_key ~cwd] is the lowercase SHA-256 digest of the absolute cwd. *)
