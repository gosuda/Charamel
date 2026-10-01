(** A program that reports the mouse events it receives.

    Upstream: [.references/bubbletea/examples/mouse/main.go]. The view turns on all-motion
    mouse reporting. Each mouse report is written as [(X: x, Y: y) name], where the name
    is the held modifier and the button, matching upstream's mouse stringer. [q], [esc]
    and [ctrl+c] quit. Upstream writes the lines above the view with [tea.Printf]; the
    port renders them in the view, because a scripted run observes only the view. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
