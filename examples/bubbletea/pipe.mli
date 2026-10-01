(** A pager for text piped into the program on standard input.

    Upstream: [.references/bubbletea/examples/pipe/main.go]. [main] reads the piped text,
    puts it in an editable single-line input, and shows it. [ctrl+c], [esc] and [enter]
    quit; every other key, [q] included, edits the text. When standard input is a terminal
    the program writes [Try piping in some text.] and exits with status 1, as upstream
    does. The port reads the piped bytes in [main] rather than in the application, so the
    smoke drives a fixed value instead of a real pipe. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
