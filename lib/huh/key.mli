(** Fresh typed keys for form results.

    A key carries a human-readable name for prompts and diagnostics, while its identity is
    a fresh [Type.Id] witness. *)

type 'a t

val v : string -> 'a t
(** [v name] creates a fresh typed key named [name]. *)

val name : 'a t -> string
(** [name key] is the diagnostic name carried by [key]. *)

val equal : 'a t -> 'b t -> bool
(** [equal a b] tests key identity, not the displayed names. *)

val uid : 'a t -> int
(** [uid key] is the process-local identity used by [Results]. *)

val id : 'a t -> 'a Type.Id.t
(** [id key] is the type witness used for safe heterogeneous lookup. *)

val pp : 'a t Fmt.t
(** [pp] formats a key's displayed name. *)
