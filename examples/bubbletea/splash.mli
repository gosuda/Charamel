(** A full-screen animated colour gradient.

    Upstream: [.references/bubbletea/examples/splash/main.go]. The view fills the
    alternate screen with half-block cells coloured from a twelve-stop gradient, rotated
    by an angle that advances with time. Any key press quits. The port advances the
    animation phase in the model on a sixty-hertz command instead of reading the wall
    clock in the view, and it shows [Initializing...] until the first size report arrives.
*)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
