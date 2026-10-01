(** A program that runs a series of commands in order.

    Upstream: [.references/bubbletea/examples/sequence/main.go]. The initial command is a
    sequence of batches and sequences, so the lines [1-1-1], [1-2-2], [1-2-1], [1-1-2],
    [2], [3-1-1], [3-1-2], [3-2-1] and [3-2-2] appear in that order, and the program then
    quits. Any key press quits early. Upstream writes the lines above the view with
    [tea.Println]; the port renders them in the view, because a scripted run observes only
    the view. Upstream sleeps inside a command before printing; the port delays the
    message that carries the line, which is the declarative shape of the same timing. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
