(** A counter application for the Tea terminal runtime. *)

type message = Key of Charamel_tea.Key.t  (** The type for counter input messages. *)

val app : (int, message) Charamel_tea.app
(** [app] is the counter application. *)

val main : unit -> unit
(** [main ()] runs the counter application against the local terminal. *)
