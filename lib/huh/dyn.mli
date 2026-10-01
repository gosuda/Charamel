(** Values which may be recomputed from the current form results. *)

type 'a t =
  | Const of 'a
  | Of_results of (Results.t -> 'a)
  | Of_results_async of (Results.t -> 'a)
      (** [Const v] never changes. [Of_results f] recomputes with [f] whenever results
          change. [Of_results_async f] computes the same value, but interactive fields
          defer [f] to a command so a slow producer does not block the update loop; the
          field shows a loading line after 25 milliseconds. *)

val const : 'a -> 'a t
(** [const value] is a dynamic value that never changes. *)

val eval : 'a t -> Results.t -> 'a
(** [eval dynamic results] computes [dynamic] against [results], synchronously for every
    variant. *)
