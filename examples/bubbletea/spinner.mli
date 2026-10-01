(** A spinner that turns until the user quits.

    Upstream: [.references/bubbletea/examples/spinner/main.go]. The spinner uses the [Dot]
    frames in color 205. The program stops on [q], [esc] or [ctrl+c]. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
