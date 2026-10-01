(** A Markdown menu rendered by Glamour inside a bordered viewport.

    Upstream: [.references/bubbletea/examples/glamour/main.go]. The menu is rendered once
    at start-up into a 78 by 20 viewport with a rounded border, and the viewport scrolls
    with the arrow keys. [q], [esc] and [ctrl+c] quit. Upstream selects the dark or light
    theme by reading the terminal background before the run; the port starts on the dark
    theme and re-renders when {!Charamel_tea.Event.Background_color} answers
    {!Charamel_tea.Cmd.query}. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
