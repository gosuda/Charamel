(** Context rules loaded from the explicitly configured context paths. *)

type rule = { path : string; globs : string list; always : bool; body : string }
(** A bounded Markdown rule. [path] is the source path used in context headings. *)

type t
(** The loaded rule index. *)

val load : fs:Eio.Fs.dir_ty Eio.Path.t -> cwd:string -> config:Config.t -> t
(** [load ~fs ~cwd ~config] reads only [config.context_paths], relative to [cwd]. Files
    and Markdown rule headers use the supported [globs] and [always] fields; malformed
    headers raise [Invalid_argument]. Missing paths are skipped. *)

val context_text : t -> string
(** [context_text t] renders all always rules as Markdown context sections. *)

val for_path : t -> string -> rule list
(** [for_path t path] returns glob rules matching the cwd-relative path. *)

val attach_text : t -> touched:string list -> string
(** [attach_text t ~touched] renders each matching glob rule once, preserving index order.
*)
