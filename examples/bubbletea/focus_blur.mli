(** A program that follows terminal focus and blur reports.

    Upstream: [.references/bubbletea/examples/focus-blur/main.go]. The frame says whether
    focus reporting is enabled and whether the window is focused or blurred. [t] toggles
    focus reporting, which also hides the focused line. [q] and [ctrl+c] quit. The view
    carries the [report_focus] flag, so reports arrive only while [t] leaves reporting on.
    The smoke injects focus and blur reports directly because a simulated terminal has no
    window manager to move in and out of focus. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
