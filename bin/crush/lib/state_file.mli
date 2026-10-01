(** Atomic private state-file replacement.

    [State_file] writes complete bytes to a unique sibling and atomically renames that
    sibling over the destination. Temporary files are exclusive, mode [0600], and removed
    by the operation that owns them on every failure or cancellation. *)

type error = [ `Io of string * string ]
(** A filesystem failure, carrying the destination path and a diagnostic. *)

val pp_error : error Fmt.t
(** [pp_error] formats a state-file error. *)

val replace : string -> string -> (unit, error) result Lwt.t
(** [replace path contents] atomically replaces [path] with [contents]. The destination
    itself is never followed as a symlink. *)
