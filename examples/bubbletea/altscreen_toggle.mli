(** An inline view that switches to the alternate screen on space.

    Upstream: [.references/bubbletea/examples/altscreen-toggle/main.go]. [space] switches
    between altscreen mode and inline mode and the frame declares [alt_screen]
    accordingly. [ctrl+z] suspends the process and the resume notification clears the
    empty suspend frame. [q], [escape] and [ctrl+c] quit with [Bye!]. The smoke scripts
    never press [ctrl+z] because [Charamel_tea.Test.run] refuses [Cmd.suspend]. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
