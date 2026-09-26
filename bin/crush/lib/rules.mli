(** Context rules loaded from the explicitly configured context paths. *)

type rule = { path : string; globs : string list; always : bool; body : string }
(** A bounded Markdown rule. [path] is the source path used in context headings. *)

type t
(** The loaded rule index. *)

val load : fs_root:string -> cwd:string -> config:Config.t -> t Lwt.t
(** [load ~fs_root ~cwd ~config] reads only [config.context_paths], relative to [cwd].
    Files and Markdown rule headers use the supported [globs] and [always] fields;
    malformed headers raise [Invalid_argument]. Missing paths are skipped. *)

val context_text : t -> string
(** [context_text t] renders all always rules as Markdown context sections. *)

val for_path : t -> string -> rule list
(** [for_path t path] returns glob rules matching the cwd-relative path. *)

val attach_text : t -> touched:string list -> string
(** [attach_text t ~touched] renders each matching glob rule once, preserving index order.
*)

val canonical_path : string -> string
(** [canonical_path path] resolves dot components lexically and drops dot-dot above the
    root. A leading slash is preserved. A dot-dot component cancels the component before
    it, whether literal or an earlier dot-dot; a leading run of dot-dot components is
    preserved. The empty path stays empty. *)

val path_is_under : root:string -> string -> bool
(** [path_is_under ~root path] is [true] when [path] equals [root] or lies beneath it at a
    component boundary. The root ["/"] contains every path. *)
