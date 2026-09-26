(** The cell-grid rasteriser: styled terminal text in, an addressable grid out.

    A grid is an array of rows, each row an array of cells. {!val:of_string} builds one
    from terminal output, keeping the printable text, the SGR attributes and the OSC 8
    hyperlinks it finds; every other sequence is dropped, because a cell cannot hold it.
    {!val:to_string} turns a grid back into terminal output, so a round trip preserves
    what a cell can carry.

    A wide grapheme occupies two cells: the first holds the grapheme with [width = 2], the
    second is its continuation, holding no text and [cont = true]. A grapheme that does
    not fit the remainder of a row continues on the next row; a grapheme wider than the
    whole grid occupies nothing. Text after the last row is dropped. *)

type cell = {
  text : string;
  width : int;
  style : Style.t;
  link : Link.t option;
  cont : bool;
}
(** The type for grid cells. [text] is one grapheme cluster, or the run of zero-width
    marks appended to the cluster before it; [width] is the number of cells it occupies,
    so a zero-width cluster has [width = 0] and lives inside its predecessor's [text].
    [cont] marks the second cell of a wide cluster, which renders nothing on its own. *)

val blank : cell
(** [blank] is the empty cell: one space, default style, no link. *)

val equal : cell -> cell -> bool
(** [equal a b] is [true] when the two cells render identically. *)

val is_blank : cell -> bool
(** [is_blank c] is [true] when [c] equals {!val:blank}. *)

val last_nonblank : cell array -> int
(** [last_nonblank line] is the index of the last cell of [line] that is not {!val:blank},
    or [-1] when every cell is blank. *)

val link_equal : Link.t option -> Link.t option -> bool
(** [link_equal a b] compares two links by URL and parameters. *)

val layout : width:int -> max_rows:int -> string -> cell array array
(** [layout ~width ~max_rows s] rasterises [s] into at most [max_rows] rows of [width]
    cells, one row per line the text produces. Rows are never padded: the result holds
    exactly the rows the text reaches. [width] or [max_rows] of zero or less yields no
    rows. *)

val pad_rows : cell array array -> rows:int -> width:int -> cell array array
(** [pad_rows grid ~rows ~width] is [grid] cut to [rows] rows, with blank rows of [width]
    cells appended until it has [rows]. *)

val of_string : width:int -> rows:int -> string -> cell array array
(** [of_string ~width ~rows s] is {!val:layout} at that size, padded with {!val:pad_rows}
    to exactly [rows] rows of [width] cells. *)

val to_string : ?trim:bool -> cell array array -> string
(** [to_string ?trim grid] is the terminal output that paints [grid]: one line per row,
    joined by newlines, with the SGR and OSC 8 transitions each cell needs. Continuation
    cells paint nothing. When [trim] is [true], and it is [false] by default, trailing
    spaces are removed from each line. *)
