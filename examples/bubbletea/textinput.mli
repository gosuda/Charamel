(** A single-line input that echoes what the user types.

    Upstream: [.references/bubbletea/examples/textinput/main.go]. The input is focused
    from the start, shows the placeholder [Pikachu], is twenty cells wide and allows one
    hundred and fifty-six characters. Typing edits the value. [enter], [esc] and [ctrl+c]
    quit. The bubble draws a real terminal cursor rather than a virtual one, and the frame
    offsets it by the header height. The port keeps upstream's unused error field out of
    the model because no message can fill it. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
