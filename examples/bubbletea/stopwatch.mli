(** An elapsed-time stopwatch with start, stop and reset keys.

    Upstream: [.references/bubbletea/examples/stopwatch/main.go]. [s] toggles the
    stopwatch, [r] resets the elapsed time, and [q] or [ctrl+c] quits. The view prefixes
    the elapsed time with [Elapsed:] and appends the short help view. The port keeps the
    upstream help flags, which the upstream program updates from the state before the
    toggle, so the [start] and [stop] labels trail the running state by one key press. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
