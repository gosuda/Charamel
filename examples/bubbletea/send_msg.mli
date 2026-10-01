(** A count of messages a detached producer sends into the program.

    Upstream: [.references/bubbletea/examples/send-msg/main.go]. A producer goroutine
    posts a message every 100 milliseconds through the program's own send handle; each one
    raises the count and a spinner turns. [q] and [ctrl+c] quit.

    Upstream's handle-based `p.Send` is the subscription seam here: the producer writes
    into an [int Lwt_stream.t] read by {!Charamel_tea.Sub.stream}. The smoke counts a
    scripted message instead, because a live producer needs the real reactor. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
