(** An ellipse of asterisks chasing the mouse across a cellular grid.

    Upstream: [.references/bubbletea/examples/cellbuffer/main.go]. The grid holds one cell
    per terminal cell. A spring moves the ellipse centre toward the pointer, so the shape
    overshoots and settles. Cell motion reporting is on, so drags and hover both move the
    target. Any key quits. The alternate screen is used.

    Upstream draws through the example's own [cellbuffer] type, which has no double-width
    support. That buffer is a module-private array here, named [Cell_buffer]; upstream's
    [harmonica.Spring] is [Charamel_harmonica.Spring] with the same frequency and damping.
*)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
