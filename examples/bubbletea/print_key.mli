(** A key inspector that names every key press.

    Upstream: [.references/bubbletea/examples/print-key/main.go]. Each key press is
    reported as [You pressed: <name>], with the printable text appended when the key
    carries one. [ctrl+c] quits. The view asks for the keyboard protocol's event-type
    reports. Upstream writes the lines above the view with [tea.Printf]; the port renders
    them in the view, because a scripted run observes only the view. Upstream also prints
    the keyboard-enhancements report when the terminal answers; the port has no such
    message, so that line is not ported. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
