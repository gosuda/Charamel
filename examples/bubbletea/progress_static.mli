(** A progress bar rendered from an explicit percentage once per second.

    Upstream: [.references/bubbletea/examples/progress-static/main.go]. The model keeps
    the percentage and the bar renders it with [view_as], so the bar never animates. The
    percentage grows by 25 percent each second and the program quits at 100 percent. Any
    key press quits. Widths above 80 cells are capped, as upstream does. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
