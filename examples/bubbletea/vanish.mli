(** A program that erases itself when it quits.

    Upstream: [.references/bubbletea/examples/vanish/main.go]. The view starts with the
    two-line prompt. Any key press blanks the view and quits, so the terminal shows no
    trace of the program. The [main] entry point discards the final model and lets
    {!Smoke.run} raise, where upstream reports the error on the standard error stream. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
