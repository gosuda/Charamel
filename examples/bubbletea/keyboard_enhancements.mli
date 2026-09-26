(** A program that reports the terminal's enhanced keyboard support.

    Upstream: [.references/bubbletea/examples/keyboard-enhancements/main.go]. The view
    requests the Kitty flags with [report_events] on, so the terminal reports key releases
    as well as presses. Pressed keys and released keys are written above the view with
    [Cmd.print]. [ctrl+c] quits. This runtime answers the startup query with a
    [Kitty_flags] terminal report rather than a keyboard-enhancements message, and a
    report of any flag sets disambiguation, as upstream does. Upstream also derives an
    unused border style from the background-color report, which this port omits. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
