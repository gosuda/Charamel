(** Crockford ULIDs for project-scoped session names.

    [Ulid] keeps timestamp and entropy generation explicit. Callers supply the current
    millisecond timestamp and a random-byte function; no process-global random state is
    consulted. *)

val v : now_ms:int -> random:(int -> string) -> string
(** [v ~now_ms ~random] returns a 26-character Crockford ULID containing the low 48 bits
    of [now_ms] followed by 80 random bits. [random] is called with [10] and must return
    exactly ten bytes. Negative or out-of-range timestamps and a wrongly sized random
    result raise [Invalid_argument]. *)

val is_valid : string -> bool
(** [is_valid id] is [true] exactly for an uppercase 26-character Crockford ULID with a
    timestamp that fits in 48 bits. *)

val timestamp_ms : string -> int option
(** [timestamp_ms id] returns the millisecond timestamp encoded by [id], or [None] for an
    invalid ULID. *)
