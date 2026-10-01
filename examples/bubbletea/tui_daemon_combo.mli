(** A background worker that also serves a terminal user interface.

    Upstream: [.references/bubbletea/examples/tui-daemon-combo/main.go]. The program runs
    a spinner over five result rows, one job at a time, and prints [Job finished in ...]
    for each finished job. Any key press quits. The [-d] flag, or a standard output that
    is not a terminal, selects the daemon shape: {!Charamel_tea.run} gets
    [~renderer:`None], so no frame is painted and each log line reaches the terminal
    through {!Charamel_tea.Cmd.print}. In the user-interface shape the log lines are
    discarded, as upstream discards them too, and the smoke asserts that view. Two details
    differ from Go. The pretend job is a {!Charamel_tea.Cmd.after} timer rather than a
    sleeping goroutine, and the first spinner tick comes from [Spinner.subscriptions]
    rather than from [Init]. Durations below one second print as whole milliseconds and
    longer ones as seconds with the trailing zeros dropped, which is how Go formats a
    [time.Duration]. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
