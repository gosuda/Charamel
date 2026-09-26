(** Local JSON key-value store.

    Each database is one JSON file [<db>.json] under a caller-supplied root directory. The
    file holds one JSON object mapping every key to a [{ "v": string, "b": bool }] entry.
    [b] is [true] and [v] is base64 when the stored bytes are not valid UTF-8, and [false]
    with [v] as the literal text otherwise. The classification is automatic
    ([String.is_valid_utf_8]); callers never choose it.

    Every write replaces the file by creating a fresh sibling temporary file and renaming
    it into place, so a reader never observes a partially written file, and a file this
    module cannot parse is never overwritten. A failed read stops before any write is
    attempted. The root directory and a missing database file are both created on the
    first successful [set].

    Every database name must be a single, non-empty, safe path component. It may not be
    ['.'], [".."], contain ['/'] or contain a NUL byte. A name containing [".."] is
    rejected to keep database paths confined to [root].

    Mutating operations serialize within the process, including calls through different
    paths to the same root. There is no cross-process locking. Concurrent processes can
    each read the same starting contents and the last rename can discard another process's
    update. Readers see a complete old or new file, not a partial write. Database reads
    are bounded at 64 MiB. Larger files produce an [Io] error. *)

type value =
  | Text of string  (** Valid UTF-8 content, stored as the literal JSON string. *)
  | Binary of string
      (** Raw bytes that are not valid UTF-8, stored base64-encoded on disk. The string
          here is the original raw content, already decoded from base64. *)

type error =
  [ `No_such_db of string
  | `No_such_key of string
  | `Corrupt of string
  | `Io of string
  | `Invalid_db of string ]
(** The type for store failures.

    [`No_such_db] names a database with no file under the root. [`No_such_key] names a key
    absent from an existing database. [`Corrupt] carries a diagnostic for a database file
    that is not the JSON shape this module writes; the file is left untouched. [`Io]
    carries a diagnostic for a filesystem failure. [`Invalid_db] names a database argument
    that is empty, is ['.'] or [".."], contains ['/'], or contains a NUL byte. *)

val pp_error : error Fmt.t
(** [pp_error] formats [error] for diagnostics. *)

val set : root:string -> db:string -> string -> string -> (unit, error) result Lwt.t
(** [set ~root ~db key data] stores [data] at [key] in [db], creating [db] and every
    missing ancestor directory of [root] if absent. An existing value at [key] is
    replaced; every other key already in [db] is preserved. [data] is stored as [Text]
    when it is valid UTF-8 and as [Binary] otherwise. *)

val get : root:string -> db:string -> string -> (value, error) result Lwt.t
(** [get ~root ~db key] is the value stored at [key] in [db]. *)

val delete : root:string -> db:string -> string -> (unit, error) result Lwt.t
(** [delete ~root ~db key] removes [key] from [db]. Every other key already in [db] is
    preserved. *)

val list : root:string -> db:string -> ((string * value) list, error) result Lwt.t
(** [list ~root ~db] is every [(key, value)] pair in [db], sorted by key with
    [String.compare]. [db] having no file is [Ok []], not an error. *)

val delete_db : root:string -> db:string -> (unit, error) result Lwt.t
(** [delete_db ~root ~db] removes [db]'s file entirely. *)

val dbs : root:string -> (string list, error) result Lwt.t
(** [dbs ~root] is the name of every database file directly under [root], sorted with
    [String.compare]. [root] not existing yet is [Ok []], not an error. *)
