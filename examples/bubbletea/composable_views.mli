(** Two bubbles side by side, with one of them focused.

    Upstream: [.references/bubbletea/examples/composable-views/main.go]. A countdown timer
    and a spinner sit in bordered boxes that swap the focused frame. [tab] moves focus to
    the other box. [n] restarts the focused bubble: the timer returns to one minute and
    the spinner advances to the next kind in the nine-spinner list. [q] and [ctrl+c] quit.
    Both bubbles tick from their own subscriptions, so the port sends no tick command at
    startup. Neither bubble binds a key, so an unhandled key changes nothing, which is
    what passing it to the focused bubble does upstream. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
