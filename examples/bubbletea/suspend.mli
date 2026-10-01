(** A program that suspends itself and redraws on resume.

    Upstream: [.references/bubbletea/examples/suspend/main.go]. [ctrl+z] suspends the
    process, [ctrl+c] interrupts it, and [q] or [esc] exits. The view is empty while the
    program is suspended or quitting. The port asks for resume reports through
    {!Charamel_tea.Sub.resume}. [ctrl+z] delivers its own message and suspends in one
    batch, because a scripted run cannot execute {!Charamel_tea.Cmd.suspend}, so the smoke
    reaches the suspended state through that message. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
