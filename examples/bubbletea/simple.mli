(** A countdown that exits by itself.

    Upstream: [.references/bubbletea/examples/simple/main.go]. The count starts at 5 and
    drops by one each second. The program stops at 0, on [q] or on [ctrl+c]; [ctrl+z]
    suspends it. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
