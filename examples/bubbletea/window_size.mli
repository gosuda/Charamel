(** A program that queries and prints the window size.

    Upstream: [.references/bubbletea/examples/window-size/main.go]. Every key that is not
    [q], [esc] or [ctrl+c] re-queries the terminal size, and each reported size is written
    as [The window size is: colsxrows]. [q], [esc] and [ctrl+c] quit. The runtime reports
    the size once at startup, so the first line appears without any key press. Upstream
    writes the lines above the view with [tea.Printf]; the port renders them in the view,
    because a scripted run observes only the view. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
