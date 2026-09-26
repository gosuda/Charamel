(** A grocery list with the stock list delegate.

    Upstream: [.references/bubbletea/examples/list-default/main.go]. The list shows twenty
    three items under the title [My Fave Things] inside a one-by-two cell margin. [↑/k]
    and [↓/j] move the cursor. [/] starts filtering and [enter] applies the filter. [g]
    and [G] jump to the first and last item. [q], [esc] and [ctrl+c] quit. The program,
    not the bubble, quits because this [List] component exposes quit bindings for its help
    line only. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
