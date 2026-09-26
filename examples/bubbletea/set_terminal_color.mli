(** A program that sets the terminal's default colours from a hex value.

    Upstream: [.references/bubbletea/examples/set-terminal-color/main.go]. The choose view
    lists Foreground, Background and Cursor; [j], [k] and the arrows move the marker, and
    [enter] opens the hex input. [enter] submits, [esc] returns to the choose view, and
    [q] or [ctrl+c] quits. A submitted colour is applied to the view's default foreground,
    default background, or requested cursor colour, and a value that does not parse shows
    an error line. The port parses with {!Charamel_ansi.Color.of_hex}, which requires the
    leading [#] that upstream's hex reader makes optional. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
