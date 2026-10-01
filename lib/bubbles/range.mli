(** Integer clamping with bounds normalization. *)

val clamp : int -> int -> int -> int
(** [clamp lo hi v] confines [v] to the closed interval between [lo] and [hi]. Reversed
    bounds are swapped, so the result always lies between [min lo hi] and [max lo hi]. *)
