(** Bounded output artifacts.

    [Artifact] keeps complete tool output in a private per-session directory and returns a
    bounded head/tail preview to model-facing consumers. *)

type t
(** The artifact store. *)

val create : fs_root:string -> dir:string -> t
(** [create ~fs_root ~dir] constructs a lazy store rooted at [dir] below [fs_root]. The
    directory is created with mode [0700] on the first successful save. *)

val save :
  t ->
  random:(int -> string) ->
  string ->
  (string, [ `Io of string * string ]) result Lwt.t
(** [save store ~random contents] writes complete [contents] to a fresh
    [art-<8 lowercase hex>.txt] file with mode [0600], returning its id. [random] is
    called with [4] and must return exactly four bytes. *)

val load :
  t ->
  id:string ->
  (string, [ `Not_found of string | `Io of string * string ]) result Lwt.t
(** [load store ~id] reads one previously saved complete artifact. Invalid ids and missing
    files return [`Not_found]. *)

val max_inline_bytes : int
(** [max_inline_bytes] is the inline threshold, [65_536] bytes. *)

val head_lines : int
(** [head_lines] is the number of preview lines retained at the beginning. *)

val tail_lines : int
(** [tail_lines] is the number of preview lines retained at the end. *)

val truncate : t -> random:(int -> string) -> string -> (string * string option) Lwt.t
(** [truncate store ~random contents] returns [(contents, None)] when the byte length is
    at most [max_inline_bytes]. Larger contents are saved in full and returned as the
    first [head_lines] and last [tail_lines] lines around an artifact reference, with
    [Some id]. *)
