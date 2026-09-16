(** Filesystem search tools.

    Directory listings, glob searches and text searches stay within the supplied
    capability and skip generated and hidden trees by default. *)

val ls : Tool.t
(** [ls] is the directory listing tool. *)

val glob : Tool.t
(** [glob] is the path globbing tool. *)

val grep : Tool.t
(** [grep] is the text search tool. *)
