(** A scrolling grayscale starfield rendered with half blocks.

    Upstream: [.references/bubbletea/examples/space/main.go]. Every cell of the field is
    one random gray pair drawn with the upper half block, and the field scrolls one column
    per tick at sixty ticks per second. [q] and [ctrl+c] quit. The frame runs on the
    alternate screen. The upstream comment says the point of the example is the repaint
    speed of the animation; the smoke asserts only the title and the field geometry,
    because the stripped frame text cannot show the colors. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
