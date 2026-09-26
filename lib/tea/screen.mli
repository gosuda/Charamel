(** The terminal cell grid and mode reconciler.

    [t] is a screen: the cell grid diffed against the previous frame, plus every terminal
    mode last applied. {!render} is the whole runtime-facing contract: given the next
    [View.t] it returns the bytes that bring the terminal from what [t] remembers to that
    view, covering both the cell diff and every declarative mode in {!View.t}. A later
    runtime module owns the terminal I/O, the render loop and its timing, and the raw
    terminal modes an OS session needs at startup; this module owns only what bytes one
    frame transition requires.

    Two geometries are supported. In the alternate screen the grid is addressed
    absolutely: row and column are terminal-wide coordinates, and {!View.t.cursor} is
    positioned there directly. Inline (the terminal's main screen, growing downward from
    wherever the cursor already is) the grid is frame-relative: row 0 is always the view's
    own first line, and every movement is a relative cursor motion, because the view's
    absolute row on the terminal is never known and is never queried. Before the first
    inline {!render} of a session, and after every {!reset} or {!clear}, the cursor must
    already sit at column 1 of its current row (for example because the caller just wrote
    ["\r\n"], or because {!clear} put it there); {!render} relies on that invariant and
    never probes cursor position itself.

    A view taller than the terminal is clipped in the alternate screen (surplus lines are
    dropped) and windowed inline (only the view's first [rows] lines are drawn; the rest
    never appears). Terminal-side insert/delete-line scroll optimisation, drift recovery
    and palette detection are not implemented: every dirty line is rewritten as one
    contiguous run, and only [Rgb] colors are sent as the default foreground or
    background. *)

type t
(** The type for screens. *)

val create : rows:int -> cols:int -> t
(** [create ~rows ~cols] is a screen over a terminal of [rows] by [cols] cells, with
    nothing yet applied. The first {!render} treats every occupied line as dirty and every
    mode [view] requests as a change from nothing. *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize t ~rows ~cols] records a terminal size change. It writes no bytes itself; the
    next {!render} repaints unconditionally to account for it: in the alternate screen it
    clears the whole display first, because terminal reflow makes the previous grid
    untrustworthy; inline it re-anchors at the cursor's current row and erases every line
    it redraws, to cover content the reflow may have shifted that the new frame does not
    overwrite. *)

val render : t -> View.t -> string
(** [render t view] is the escape sequence that transitions the terminal from the state
    [t] last applied to [view]: entering or leaving the alternate screen, every changed
    declarative mode (mouse reporting, bracketed paste, focus reporting, window title,
    default foreground and background, the OS progress indicator, the Kitty keyboard
    flags), the minimal cell diff for [view.content], and the application cursor's
    visibility, shape, color and position. The whole frame is wrapped in
    synchronized-output mode (2026) unless nothing changed at all, in which case [render]
    returns [""].

    [view.content] is parsed for printable text, SGR (`m`) and OSC 8 hyperlinks only;
    every other escape sequence it contains is dropped. A [Basic] or [Indexed]
    {!Charamel_ansi.Color.t} in [view.background], [view.foreground] or [view.cursor]'s
    [color] has no portable OSC 10/11/12 spelling and is silently not applied; only [Rgb]
    is sent. A cursor position that lands on the continuation cell of a wide glyph snaps
    back onto the glyph itself. *)

val clear : t -> string
(** [clear t] is the escape sequence that erases the inline region [t] currently owns and
    leaves the cursor at that region's first row, column 1, with the diff state reset so
    the next {!render} treats it as a fresh anchor. It is the seam a [Cmd.print]
    implementation needs: emit [clear t], write the printed lines (each newline
    terminated, so the cursor ends at column 1 of a fresh row), then call {!render} to
    redraw the view below them. [clear t] carries no mode bytes and does not touch [t]'s
    terminal-mode state. [clear t] is [""] when [t] is currently showing the alternate
    screen (where printed output queues until exit, by design) or does not yet own an
    inline region. *)

val reset : t -> unit
(** [reset t] forgets the cell grid and every applied terminal mode, as if [t] were
    freshly {!create}d at its current size. Call it after a foreign process ran on the
    terminal (a suspended program, or a released-and-restored [Cmd.exec]). The next
    {!render} re-anchors inline at the cursor's current row (the caller must again have
    left it at column 1) and re-emits every mode [view] requests, which is safe because
    setting an already-set DEC mode is a no-op. *)

val restore : t -> string
(** [restore t] is the escape sequence that leaves the terminal in a clean, default state
    for exit: it leaves the alternate screen if [t] is showing it, disables every mouse,
    paste and focus mode [t] applied, shows the cursor with the default shape, resets the
    default foreground and background and the progress indicator, closes any open OSC 8
    hyperlink, resets SGR attributes to default, and pops every outstanding Kitty keyboard
    protocol stack entry [t] pushed. [restore t] also calls {!reset} on [t]. *)
