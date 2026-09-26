(** A tab strip whose active tab selects the panel below it.

    Upstream: [.references/bubbletea/examples/tabs/main.go]. [tab], [right], [l] and [n]
    select the next tab. [shift+tab], [left], [h] and [p] select the previous tab. The
    index stops at the first and the last tab. [q] and [ctrl+c] quit. The styles use the
    dark highlight color, as upstream's default does. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
