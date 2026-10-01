(** A plain list that returns the chosen item.

    Upstream: [.references/bubbletea/examples/list-simple/main.go]. [up] and [down] move
    the cursor. [enter] chooses the selected item and quits with
    ["<item>? Sounds good to me."]. [q] and [ctrl+c] quit with
    ["Not hungry? That’s cool."]. The list has no status bar and no filtering. A custom
    delegate numbers the items and marks the selected one with [>]. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
