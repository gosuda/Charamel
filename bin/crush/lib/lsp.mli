(** Language server protocol client.

    [Lsp] manages language servers over bounded JSON-RPC streams. Servers start on demand
    for files in the configured project and remain available until the owning switch is
    cancelled or {!stop_all} is called. *)

type diagnostic = {
  path : string;
  line : int;
  col : int;
  severity : [ `Error | `Warning | `Info | `Hint ];
  message : string;
  source : string option;
}
(** A diagnostic reported for a source file. Lines and columns are one-based UTF-16
    positions. *)

type location = { path : string; line : int; col : int; end_line : int; end_col : int }
(** A source range. Lines and columns are one-based UTF-16 positions. *)

type symbol = { name : string; kind : string; range : location; children : symbol list }
(** A document symbol and its nested symbols. *)

type text_edit = { range : location; new_text : string }
(** A replacement of a source range with UTF-8 text. *)

type server_state =
  | Not_started
  | Starting
  | Ready
  | Failed of string
  | Disabled  (** The lifecycle state of a configured language server. *)

type error =
  [ `No_server of string
  | `Not_ready of string
  | `Rpc of string * string
  | `Timeout of string
  | `Io of string * string ]
(** Errors returned by language server operations. *)

type t
(** The type for a language server collection. *)

val defaults : (string * Config.lsp) list
(** [defaults] is the built-in language server table. File types omit their leading dot.
*)

val create :
  sw:Eio.Switch.t ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cwd:string ->
  config:Config.t ->
  t
(** [create ~sw ~proc_mgr ~clock ~fs ~cwd ~config] is a client collection. No child
    process is started by this function. *)

val servers : t -> (string * server_state) list
(** [servers t] is the configured server name and state list. *)

val handles : t -> path:string -> string option
(** [handles t ~path] is the first configured server that supports [path], or [None] when
    no server supports its extension or the path is outside the project. *)

val normalize_path : cwd:string -> string -> string
(** [normalize_path ~cwd path] resolves [path] against [cwd] and every dot and dot-dot
    component lexically. Dot-dot above the root is dropped. The result is absolute. *)

val inside : cwd:string -> string -> string -> bool
(** [inside ~cwd root path] is [true] when the normalized [path] equals the normalized
    [root], when [root] is ["/"], or when [path] lies beneath [root] at a component
    boundary. *)

val touch : t -> path:string -> unit
(** [touch t ~path] makes the selected server observe the current contents of [path]. The
    first observation sends [textDocument/didOpen]. Later observations send a full-text
    [textDocument/didChange]. Paths outside the project and paths without a configured
    server are ignored. *)

val diagnostics : t -> path:string -> wait:float -> diagnostic list
(** [diagnostics t ~path ~wait] is the latest diagnostic set for [path] after waiting at
    most [wait] seconds for a publication. A publication is held for the batching window
    before it is returned. *)

val definition : t -> path:string -> line:int -> col:int -> (location list, error) result
(** [definition t ~path ~line ~col] requests definitions at the given one-based UTF-16
    position. *)

val references : t -> path:string -> line:int -> col:int -> (location list, error) result
(** [references t ~path ~line ~col] requests references at the given one-based UTF-16
    position. *)

val document_symbols : t -> path:string -> (symbol list, error) result
(** [document_symbols t ~path] requests symbols for [path]. *)

val find_symbol : t -> path:string -> name:string -> (symbol option, error) result
(** [find_symbol t ~path ~name] finds an exact symbol name first, then a qualified name
    ending in [name]. The search is depth first. *)

val rename :
  t ->
  path:string ->
  line:int ->
  col:int ->
  new_name:string ->
  ((string * text_edit list) list, error) result
(** [rename t ~path ~line ~col ~new_name] requests a workspace edit. The returned edits
    are grouped by absolute file path and are not applied. *)

val apply_edits :
  cwd:string ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  (string * text_edit list) list ->
  (string list, error) result
(** [apply_edits ~cwd ~fs edits] applies non-overlapping edits from the bottom of each
    file upward. Relative paths resolve against [cwd]. UTF-16 positions are converted to
    UTF-8 boundaries before writing. The returned list contains changed paths. *)

val restart : t -> name:string option -> (string list * string list, error) result
(** [restart t ~name] stops and starts one named server, or all servers when [name] is
    [None]. The result contains restarted and failed names. *)

val stop_all : t -> unit
(** [stop_all t] requests graceful shutdown of every child and then releases all process
    streams. *)

val pp_error : error Fmt.t
(** [pp_error] formats an LSP error. *)
