(** A frame that drives the OS progress indicator.

    Upstream: [.references/bubbletea/examples/progress-bar/main.go]. The bar starts
    indeterminate at fifty percent. [up] and [k] raise the value by ten, [down] and [j]
    lower it by ten, [left] and [h] step the state down and [right] and [l] step it up,
    clamped to the five states. [q] and [ctrl+c] quit. The state and value reach the
    terminal through the frame's progress field, so they leave no trace in the recorded
    text; the smoke asserts the reflow the resize report causes instead. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
