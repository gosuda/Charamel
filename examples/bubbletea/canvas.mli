(** Two rounded cards and a footer composed with layers, with a key that swaps them.

    Upstream: [.references/bubbletea/examples/canvas/main.go]. Any key swaps which card is
    drawn on top in the area where the cards overlap, so the clipped half of each word
    changes. [q], [escape] and [ctrl+c] quit and leave an empty frame.

    The port composes through {!Charamel_lipgloss.Compositor} and
    {!Charamel_lipgloss.Layer}, as upstream does. Upstream stores the window width from
    the size report and never reads it; this port keeps no such field, so the frame is the
    composition's own bounds. Upstream's [charmtone] colors are the two literals this file
    pins. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
