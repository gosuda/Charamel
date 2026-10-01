(** A program that sets the terminal window title.

    Upstream: [.references/bubbletea/examples/set-window-title/main.go]. The view requests
    the title ["Hello, Bubble Tea"] and says the title is cleared on exit. Any key press
    quits. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
