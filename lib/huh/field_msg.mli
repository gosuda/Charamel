(** Existentially typed messages produced by one field's commands. *)

type t = Any : 'a Type.Id.t * 'a -> t

val inject : 'a Type.Id.t -> 'a -> t
(** [inject id message] hides the message type behind [id]. *)

val project : 'a Type.Id.t -> t -> 'a option
(** [project id message] recovers [message] when the identifiers agree. *)
