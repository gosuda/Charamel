(** Several text areas side by side, with one editor per column.

    Upstream: [.references/bubbletea/examples/split-editors/main.go]. [tab] moves focus to
    the next editor, [shift+tab] to the previous one, [ctrl+n] adds an editor up to six,
    [ctrl+w] removes the last one down to one, and [esc] or [ctrl+c] quits. Each editor is
    as wide as the terminal divided by the number of editors, and is as tall as the
    terminal less the five help lines. The help line lists the enabled bindings only, so
    [ctrl+n] leaves it at six editors and [ctrl+w] leaves it at one. Three details differ
    from Go. The editors keep their virtual cursor, so no real terminal cursor is
    requested, which is what upstream's [Cursor] returns. A cursor message carries the
    index of the editor that produced it, because [Textarea.subscriptions] is per editor.
    [Init] returns no command, because the blink arrives through the subscriptions
    instead. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
