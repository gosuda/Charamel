(** Common validators for form fields. *)

val not_empty : string -> (unit, string) result
(** [not_empty value] rejects the empty string. *)

val min_length : int -> string -> (unit, string) result
(** [min_length n value] counts Unicode scalars and requires at least [n]. *)

val max_length : int -> string -> (unit, string) result
(** [max_length n value] counts Unicode scalars and allows at most [n]. *)

val length : min:int -> max:int -> string -> (unit, string) result
(** [length ~min ~max value] applies the minimum check before the maximum check. *)

val one_of : string list -> string -> (unit, string) result
(** [one_of values value] accepts only an exact member of [values]. *)

val all : ('a -> (unit, string) result) list -> 'a -> (unit, string) result
(** [all validators value] returns the first validation error, if any. *)
