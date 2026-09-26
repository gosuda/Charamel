(** A help view that toggles between its short and full forms.

    Upstream: [.references/bubbletea/examples/help/main.go]. The frame shows the last
    arrow key chosen, or [Waiting for input...] before one is. [up], [k], [down], [j],
    [left], [h], [right] and [l] record an arrow. [?] switches the help bubble between the
    short line and the two-column full list. [q], [esc] and [ctrl+c] quit and leave [Bye!]
    on screen. The help width follows the terminal size, as upstream sets it from the
    window size message. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
