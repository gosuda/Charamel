(** A program that hands the terminal to an editor.

    Upstream: [.references/bubbletea/examples/exec/main.go]. [e] opens the editor named by
    [$EDITOR], or [vi], and returns to this program when it exits. [a] toggles the
    alternate screen and [q] or [ctrl+c] quits. A non-zero exit status is shown as
    [exit status N], where upstream shows the error text of the child process. The smoke
    scripts the editor result as a message instead of pressing [e], because
    {!Charamel_tea.Test.run} refuses {!Charamel_tea.Cmd.exec}. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
