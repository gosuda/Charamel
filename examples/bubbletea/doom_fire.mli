(** A Doom-style fire animation drawn with half-block cells.

    Upstream: [.references/bubbletea/examples/doom-fire/main.go]. The bottom row of a
    doubled grid starts at maximum intensity. Every frame cools cells upward with a random
    sideways step. [q] and [ctrl+c] quit. The alternate screen is used.

    Upstream measures elapsed wall time; the scripted clock is not readable from the
    model, so this port accumulates the frame step and the elapsed figure counts simulated
    frames. Upstream re-initialises the grid on every window-size report and this port
    does the same. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
