(** Filesystem tools and hashline patches.

    The module exposes read, write and edit tools. Patch operations use original line
    numbers and are validated before a file is changed. *)

module Patch : sig
  type op =
    | Put of { first : int; last : int; body : string list }
    | Insert_before of { line : int; body : string list }
    | Insert_after of { line : int; body : string list }
    | Cut of { first : int; last : int }  (** The type for hashline patch operations. *)

  type patch = { path : string; tag : string; ops : op list }
  (** The type for a hashline patch. *)

  val parse : string -> (patch, string) result
  (** [parse source] is the patch encoded by [source]. *)

  val apply : string -> patch -> (string * int * int, string) result
  (** [apply content patch] is the changed content, added line count and removed line
      count obtained by applying [patch] to [content]. The source content is not changed.
  *)
end

val read : Tool.t
(** [read] is the read tool. It returns selected UTF-8 file lines with a hashline snapshot
    tag. *)

val write : Tool.t
(** [write] is the write tool. It creates or replaces a UTF-8 file after the file has been
    read during the current session when it already exists. *)

val edit : Tool.t
(** [edit] is the hashline edit tool. It applies one validated patch atomically and
    reports the resulting snapshot tag. *)
