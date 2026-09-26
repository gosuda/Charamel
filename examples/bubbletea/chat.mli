(** A chat room: a transcript viewport above a text area.

    Upstream: [.references/bubbletea/examples/chat/main.go]. The transcript starts with
    the welcome lines and the input shows the placeholder [Send a message...]. Typing
    edits the draft. [enter] appends ["You: <draft>"] to the transcript, scrolls to the
    bottom and clears the draft, so [enter] cannot insert a newline. [esc] and [ctrl+c]
    print the draft and quit. The viewport and the input follow the terminal width, and
    the input keeps three lines. The [err] field of upstream is left out because nothing
    in upstream ever sets it. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
