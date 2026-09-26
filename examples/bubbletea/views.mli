(** An application that switches between two views.

    Upstream: [.references/bubbletea/examples/views/main.go]. The first view selects one
    of four tasks with [j], [k], [up] and [down], and [enter] moves to the second view,
    which plays a bouncing download bar and then exits. [q], [esc] and [ctrl+c] exit from
    either view, and the first view also exits when its ten second countdown ends. The
    port blends the progress ramp with {!Charamel_lipgloss.Blending.blend1d}, which
    interpolates in CIE L*a*b* like the upstream ramp. The upstream program quits on a
    wall-clock tick chain; the port re-arms a one second and a one sixty-second command,
    which is the declarative shape of the same two loops. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
