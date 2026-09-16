(** Values which may be recomputed from the current form results. *)

type 'a t = Const of 'a | Of_results of (Results.t -> 'a)

val const : 'a -> 'a t
(** [const value] is a dynamic value that never changes. *)

val eval : 'a t -> Results.t -> 'a
(** [eval dynamic results] computes [dynamic] against [results]. *)
