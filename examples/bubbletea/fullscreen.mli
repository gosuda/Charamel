(** An alternate-screen countdown.

    Upstream: [.references/bubbletea/examples/fullscreen/main.go]. The view shows five
    seconds before exit. [q], [esc], and [ctrl+c] quit early. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
