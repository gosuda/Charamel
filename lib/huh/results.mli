(** A typed heterogeneous collection of committed form values. *)

type t

val empty : t
(** [empty] is the collection with no values. *)

val get : 'a Key.t -> t -> 'a option
(** [get key results] returns the value stored for [key], when one exists. *)

val get_exn : 'a Key.t -> t -> 'a
(** [get_exn key results] returns the value, or raises [Invalid_argument] when unset. *)

val add : 'a Key.t -> 'a -> t -> t
(** [add key value results] replaces [key]'s value and advances its version. *)

val mem : 'a Key.t -> t -> bool
(** [mem key results] is [true] when [key] has a value. *)

val version : t -> int
(** [version results] increases after every [add]. *)
