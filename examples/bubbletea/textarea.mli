(** A multi-line editing area with a placeholder.

    Upstream: [.references/bubbletea/examples/textarea/main.go]. The area is focused from
    the start and shows the placeholder [Once upon a time...]. Typing edits the buffer and
    [enter] breaks a line. [esc] blurs the area, and the next typed key focuses it again.
    [ctrl+c] quits. The frame asks the terminal for its background colour and restyles the
    area for a light or a dark background when it answers. The port drops upstream's
    unused error field because no message can fill it. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
