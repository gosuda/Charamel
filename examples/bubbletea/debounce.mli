(** A program that debounces a command until the keys stop.

    Upstream: [.references/bubbletea/examples/debounce/main.go]. Every key press raises
    the press counter and re-arms a one second command carrying the counter. A message
    whose counter is stale is ignored; the current one quits. The view shows the press
    count and the instruction line. [ctrl+c] is an ordinary press here, as upstream gives
    it no special handling. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
