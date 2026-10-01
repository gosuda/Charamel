(** A menu that hands its selection back to the caller.

    Upstream: [.references/bubbletea/examples/result/main.go]. [up], [k], [down] and [j]
    move the marker around the three tea choices and wrap at both ends. [enter] records
    the marked choice and quits. [q], [esc] and [ctrl+c] quit without a choice. After the
    run, [main] prints the recorded choice, which is why it uses {!Smoke.run} rather than
    {!Smoke.run_}. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
