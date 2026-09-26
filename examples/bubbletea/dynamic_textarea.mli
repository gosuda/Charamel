(** A text area that grows with its content.

    Upstream: [.references/bubbletea/examples/dynamic-textarea/main.go]. The area starts
    three lines tall and grows to at most fifteen lines. Typing adds text; [enter] adds a
    line. A status line reports height, line count, cursor position and scroll percentage.
    [ctrl+c] quits. The program queries the terminal background color at startup and
    adopts the matching palette when the terminal answers. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
