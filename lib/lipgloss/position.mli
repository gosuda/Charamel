(** Relative positions for alignment.

    A position is clamped to the closed interval [0, 1]. *)

type t = private float

val v : float -> t
(** [v x] is [x] clamped to [0.0, 1.0]. *)

val left : t
(** [left] is the left or top edge, at position [0.0]. *)

val top : t
(** [top] is the top edge, at position [0.0]. *)

val center : t
(** [center] is the midpoint, at position [0.5]. *)

val right : t
(** [right] is the right edge, at position [1.0]. *)

val bottom : t
(** [bottom] is the bottom edge, at position [1.0]. *)

val to_float : t -> float
(** [to_float p] is the fractional position represented by [p]. *)
