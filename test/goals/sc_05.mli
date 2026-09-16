(** SC-05 goal harness case.

    [run ()] runs the real [glow] binary against a markdown fixture whose only content is
    one H1, forcing the dark theme and terminal color settings, and requires that the H1
    text is present and carries the dark theme's H1 foreground and background SGR
    parameters. It fails when the process exits non-zero or the styling is absent. *)

val run : unit -> unit
