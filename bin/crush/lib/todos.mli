(** Todo state and rendering.

    Todo values preserve the model-visible order and expose explicit lifecycle transitions
    for callers that update one item at a time. *)

type status =
  | Pending
  | In_progress
  | Completed  (** The type for todo lifecycle states. *)

type item = { content : string; status : status; active_form : string }
(** The type for one todo item. [active_form] is shown while the item is in progress. *)

type t
(** The type for mutable todo state. *)

val create : unit -> t
(** [create ()] is an empty todo state. *)

val set : t -> item list -> unit
(** [set t items] replaces the todo state with [items]. The list order is retained and the
    caller's list is not stored by reference. *)

val get : t -> item list
(** [get t] is a snapshot of the current todo state in insertion order. *)

type transition_error = [ `Invalid_index of int | `Invalid_transition of status * status ]
(** The type for rejected item transitions. *)

val transition : t -> index:int -> status -> (unit, transition_error) result
(** [transition t ~index status] moves the item at [index] to [status]. Pending items may
    move to [In_progress], and in-progress items may move to [Completed]. Repeating a
    state is accepted. *)

val jsont : item list Jsont.t
(** [jsont] maps todo lists to JSON arrays with [content], [status], and [active_form]
    members. *)

val render : item list -> string
(** [render items] is one line per item using [ ] for pending, [>] for active, and [x] for
    completed items. *)
