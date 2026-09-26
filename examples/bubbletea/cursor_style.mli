(** A cursor whose shape and blink state follow the arrow keys.

    Upstream: [.references/bubbletea/examples/cursor-style/main.go]. [left] and [h] step
    the cursor shape backwards through block, bar and underline. [right] and [l] step it
    forwards. Every key press flips the blink state, so the description alternates between
    [blinking] and [steady]. [q] and [ctrl+c] quit. The requested shape sits at row [0],
    column [2] of the frame, as upstream places it. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
