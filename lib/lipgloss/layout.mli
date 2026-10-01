(** Escape-aware terminal layout primitives.

    Widths are measured in terminal cells. ANSI control sequences do not occupy cells,
    Unicode extended grapheme clusters are never split, and a line break is [\n]. *)

val width : string -> int
(** [width s] is the greatest cell width of any [\n]-delimited line in [s]. *)

val height : string -> int
(** [height s] is the number of lines in [s], including the line after a trailing [\n]. *)

val size : string -> int * int
(** [size s] is [(width s, height s)]. *)

val join_horizontal : ?pos:Position.t -> string list -> string
(** [join_horizontal ?pos blocks] joins blocks side by side.

    Shorter blocks are aligned along their vertical axis at [pos], where [0.0] is the top
    and [1.0] is the bottom. Missing cells are filled with plain spaces. [pos] defaults to
    {!Position.top}. *)

val join_vertical : ?pos:Position.t -> string list -> string
(** [join_vertical ?pos blocks] joins blocks one below another.

    Each line is padded to the widest block at [pos], where [0.0] is the left and [1.0] is
    the right. [pos] defaults to {!Position.left}. *)

val place_horizontal :
  ?whitespace:string * Style.t -> width:int -> pos:Position.t -> string -> string
(** [place_horizontal ?whitespace ~width ~pos text] places each line in a field of [width]
    cells. It is unchanged when [width] is no greater than the widest input line. The
    whitespace pattern defaults to a plain space and cycles by grapheme; a grapheme that
    does not fit the final partial field is replaced by spaces, then the pattern style is
    applied to the complete fill. *)

val place_vertical :
  ?whitespace:string * Style.t -> height:int -> pos:Position.t -> string -> string
(** [place_vertical ?whitespace ~height ~pos text] places [text] in a field of [height]
    lines. It is unchanged when [height] is no greater than the input line count. Filler
    lines have the input's widest cell width and use the same whitespace option as
    {!place_horizontal}. *)

val place :
  ?h:Position.t ->
  ?v:Position.t ->
  ?whitespace:string * Style.t ->
  width:int ->
  height:int ->
  string ->
  string
(** [place ?h ?v ?whitespace ~width ~height text] applies horizontal placement followed by
    vertical placement. [h] defaults to {!Position.left}; [v] defaults to {!Position.top}.
*)

type rune_index = [ `Grapheme | `Scalar ]
(** The unit an index list counts: an extended grapheme cluster or a Unicode scalar. *)

val style_runes :
  ?basis:rune_index -> Style.t -> Style.t -> string -> indices:int list -> string
(** [style_runes ?basis matched unmatched text ~indices] styles the clusters at [indices]
    in the escape-stripped text with [matched] and every other cluster with [unmatched].
    Indices outside the text are ignored. Escape sequences remain in the result and no
    grapheme cluster is split.

    [basis] says what [indices] count. [{!rune-index:`Grapheme}] (the default) counts
    clusters, which is what {!Charamel_bubbles.Fuzzy} produces. [{!rune-index:`Scalar}]
    counts Unicode scalars, matching upstream's rune indexing: a scalar inside a
    multi-scalar cluster styles the whole cluster. *)

val wrap : ?breakpoints:string -> width:int -> string -> string
(** [wrap ?breakpoints ~width text] word-wraps [text] at [width] cells like
    {!Charamel_ansi.Text.wrap} and then rewrites every inserted line break so the line
    that follows it starts in the same state the line before it ended in: the active SGR
    attributes are reset and any open OSC 8 link is closed before the newline, then the
    link and the attributes are re-opened after it. A trailing reset closes a state left
    open at the end of [text]. Widths of at most [1] wrap nothing. [breakpoints] is passed
    to {!Charamel_ansi.Text.wrap} and defaults to empty. *)

val style_ranges : (int * int * Style.t) list -> string -> string
(** [style_ranges ranges text] applies each style to its half-open terminal-cell range.
    Ranges are consumed in the supplied order and should not overlap. Escape sequences
    outside a styled range remain byte-for-byte in place; boundaries are clipped at
    grapheme clusters by the underlying escape-aware slicer. *)
