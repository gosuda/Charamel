(** A five-second countdown with start, stop and reset keys.

    Upstream: [.references/bubbletea/examples/timer/main.go]. The timer runs on a one
    millisecond interval. [s] toggles it, [r] resets the remaining time to five seconds,
    and [q] or [ctrl+c] quits. The view prefixes the remaining time with [Exiting in] and
    appends the short help view. The port has no timeout message, so the update watches
    {!Charamel_bubbles.Timer.timed_out} and quits when it flips. Upstream disables the
    start binding while the timer runs; the port keeps that flag in the model. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
