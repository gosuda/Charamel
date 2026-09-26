(** A grocery list with a custom delegate, toggles and status messages.

    Upstream: [.references/bubbletea/examples/list-fancy/main.go],
    [.references/bubbletea/examples/list-fancy/delegate.go] and
    [.references/bubbletea/examples/list-fancy/randomitems.go]. The list starts with
    twenty four generated grocery items under the title [Groceries]. [a] adds an item at
    the top. [enter] reports the chosen item in the status bar. [x] and [backspace] delete
    the selected item. [s] toggles the spinner. [T] toggles the title bar, the filter and
    filtering. [S] toggles the status bar. [P] toggles pagination. [H] toggles the help
    menu. [/] filters and [enter] applies the filter. [↑/k] and [↓/j] move the cursor.
    [q], [esc] and [ctrl+c] quit.

    Three divergences. The upstream item generator shuffles its title and description
    pools with [math/rand] once at startup; this port walks the pools in their source
    order so the smoke is deterministic. The delegate choose and delete keys are matched
    by the program rather than by [List.delegate.update] because that hook returns a list
    message, not the status-message command upstream builds inside it; the key sets are
    disjoint, so the observed behavior matches. The program also quits on [q] and [esc]
    because this [List] component exposes those bindings for its help line only. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
