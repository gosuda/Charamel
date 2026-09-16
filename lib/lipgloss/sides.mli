(** Four-sided integer dimensions.

    Side values represent cells for padding, margins, or frame edges. *)

type t = { top : int; right : int; bottom : int; left : int }

val all : int -> t
(** [all n] sets every side to [n]. *)

val xy : x:int -> y:int -> t
(** [xy ~x ~y] sets horizontal sides to [x] and vertical sides to [y]. *)

val v : ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> unit -> t
(** [v ?top ?right ?bottom ?left ()] constructs sides. Unspecified values default to [0].
*)
