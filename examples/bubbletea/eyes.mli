(** Two large eyes that blink on a random interval.

    Upstream: [.references/bubbletea/examples/eyes/main.go]. The eyes are filled ellipses
    on the alternate screen. A tick every 50 milliseconds drives a 20-frame blink with
    easing; the eyes stay open for one to four seconds, chosen at random, and a completed
    blink has a one-in-ten chance of queueing a second blink after 300 milliseconds. A
    terminal resize recenters the eyes. [esc] and [ctrl+c] quit. The port counts elapsed
    time by accumulating the tick interval, because the simulated clock is not readable
    from the model. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
