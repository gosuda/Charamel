(** Discovery and bounded access to user-authored skills. *)

type skill = { name : string; description : string; dir : string; body : string }
(** A loaded skill. [body] excludes the optional frontmatter header. *)

type t
(** The immutable skill index. *)

val load : fs_root:string -> config:Config.t -> home:string -> t Lwt.t
(** [load ~fs_root ~config ~home] reads each configured skill directory followed by
    [home/.crush/skills]. Missing directories are skipped. Files larger than the bounded
    skill-file limit are not indexed. Invalid supported frontmatter raises
    [Invalid_argument] with its path and field. *)

val find : t -> string -> skill option
(** [find t name] returns the first skill with [name]. *)

val all : t -> skill list
(** [all t] returns skills in directory and lexical discovery order. *)

val index_text : t -> string
(** [index_text t] renders the compact name/description index used by prompts. *)

val resolve_uri :
  t -> fs_root:string -> string -> (string, [ `Not_found of string ]) result Lwt.t
(** [resolve_uri t ~fs_root uri] resolves [skill://name] to its body or
    [skill://name/path] to a bounded file below that skill directory. Traversal components
    are rejected, and a link that leaves the skill directory is not read. *)
