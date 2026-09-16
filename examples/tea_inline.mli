(** An inline Tea application that prints output above a live view. *)

type message =
  | Tick of Mtime.t
  | Finished
  | Key of Charm_tea.Key.t  (** The type for inline application messages. *)

val app : (int, message) Charm_tea.app
(** [app] is the inline output application. *)

val main : unit -> unit
(** [main ()] runs the inline output application against the local terminal. *)
