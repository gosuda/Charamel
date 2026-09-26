(** A count of events arriving from outside the program, animated by a spinner.

    Upstream: [.references/bubbletea/examples/realtime/main.go]. A background process
    posts events at an irregular interval and each one raises the count. Any key quits.

    Upstream pushes into a Go channel that [waitForActivity] consumes; the subscription
    seam here is {!Charamel_tea.Sub.stream}, so the external source is an
    [unit Lwt_stream.t] and the re-arm loop is not needed. The smoke delivers the activity
    message directly, because the scripted clock does not run a live producer and a stream
    reader sees nothing until a real reactor pumps it. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
