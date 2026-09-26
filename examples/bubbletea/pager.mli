(** A text pager with a header, a footer and a scrolling content viewport.

    Upstream: [.references/bubbletea/examples/pager/main.go]. The viewport fills the
    terminal between the header and the footer. It numbers every content line in a left
    gutter and highlights each occurrence of [artichoke]. [f], [space] and [pgdn] page
    down, [b] and [pgup] page up, [d] and [u] move half a page, [j] and [down] move one
    line down, [k] and [up] move one line up, and [h] and [l] scroll sideways. The mouse
    wheel scrolls the content. [q], [escape] and [ctrl+c] quit. The frame uses the
    alternate screen and cell-motion mouse reporting. The selected occurrence is visible
    only as color, so the scripted frames assert the scroll position that selecting it
    moves instead.

    Upstream reads [artichoke.md] from the example directory. This port carries the text
    as a constant, because an example must not depend on a file next to the executable.
    Upstream sets the viewport's [YPosition] to the header height. This runtime has no
    such property, so the header and footer are joined above and below the viewport in the
    rendered frame instead. Upstream finds the highlights with a regular expression for
    one literal word; this port searches for the same literal word as a substring. Byte
    offsets become grapheme ranges through
    {!Charamel_bubbles.Viewport.grapheme_ranges_of_byte_ranges}. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
