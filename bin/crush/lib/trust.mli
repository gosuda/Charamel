(** Trust records for project configuration.

    A record names one project configuration file by its canonical path and the SHA-256
    digest of its bytes. A file is trusted only while a record holds both its current
    canonical path and its current digest, so any edit to the file revokes its trust. The
    store is one private JSON file in the user's configuration home, written only by an
    explicit grant. *)

type record = { path : string; digest : string }
(** One trusted file: its canonical path and the lowercase SHA-256 hex digest of its
    bytes. *)

type t
(** A set of records holding at most one digest per path. *)

type error = [ `Parse of string * string | `Io of string * string ]
(** A store failure. The first component is the store path. *)

val digest : string -> string
(** [digest bytes] is the lowercase SHA-256 hex digest of [bytes]. *)

val load : fs_root:string -> string -> (t, [> error ]) result Lwt.t
(** [load ~fs_root path] reads the store at [path] under [fs_root]. A missing store is
    empty. A store that does not decode is an error, never an empty set. *)

val mem : t -> record -> bool
(** [mem t record] holds when [t] records [record.digest] for [record.path]. *)

val add : t -> record list -> t
(** [add t records] records each of [records], replacing the digest any earlier record
    held for the same path. *)

val save : fs_root:string -> string -> t -> (unit, [> error ]) result Lwt.t
(** [save ~fs_root path t] creates the parent directory when needed and atomically
    replaces the store at [path] with a mode [0600] file. *)
