(** Configurable text tables.

    A table is an immutable value built through {!val:v} and turned into text by
    {!val:render}. Column widths are resolved from the cells' content and the cell styles,
    then expanded or shrunk toward the requested table width; rows are windowed by
    {!val:v}'s [height] and [offset]. *)

type t
(** The type for tables. *)

type row = string list
(** The type for one row: one cell per column. Short rows read as empty cells. *)

module Data : sig
  (** Table rows as a first-class value, so one source can feed different tables and a
      caller can query what a table will render. *)

  type t
  (** The type for row sources. *)

  val rows : string list list -> t
  (** [rows content] is the source holding [content]. *)

  val at : t -> row:int -> col:int -> string
  (** [at data ~row ~col] is the cell at those indexes, or the empty string when either is
      out of range. *)

  val row_count : t -> int
  (** [row_count data] is the number of rows. *)

  val columns : t -> int
  (** [columns data] is the width of the longest row. *)

  val append : t -> row -> t
  (** [append data row] adds [row] at the end. *)

  val filter : t -> (int -> bool) -> t
  (** [filter data keep] keeps the rows whose zero-based index satisfies [keep], in order.
  *)

  val matrix : t -> string list list
  (** [matrix data] is the rows as a plain list of lists. *)
end

type style_func = row:int -> col:int -> Style.t
(** The type for cell styles. [row] is [-1] for the header row and the zero-based data
    index otherwise. *)

val v :
  ?headers:string list ->
  ?data:Data.t ->
  ?rows:string list list ->
  ?border:Border.t ->
  ?border_top:bool ->
  ?border_bottom:bool ->
  ?border_left:bool ->
  ?border_right:bool ->
  ?border_header:bool ->
  ?border_column:bool ->
  ?border_row:bool ->
  ?base_style:Style.t ->
  ?border_style:Style.t ->
  ?style:style_func ->
  ?width:int ->
  ?fit_content:bool ->
  ?height:int ->
  ?offset:int ->
  ?wrap:bool ->
  unit ->
  t
(** [v ?headers ?data ?rows ?border ?border_top ?border_bottom ?border_left ?border_right
     ?border_header ?border_column ?border_row ?base_style ?border_style ?style ?width
     ?fit_content ?height ?offset ?wrap ()] is a table.

    - [data] holds the rows; [rows] is shorthand for [~data:(Data.rows rows)], and [data]
      wins when both are given. Both default to no rows.
    - [headers] defaults to no header row.
    - [border] defaults to [Border.normal]. The six toggles [border_top], [border_bottom],
      [border_left], [border_right], [border_header] and [border_column] default to [true]
      and [border_row] defaults to [false], as upstream does. A toggle suppresses its line
      or separator whatever the border glyphs hold; a line whose glyphs are all empty
      renders nothing even when its toggle is [true], which is how [Border.none] and a
      markdown-style border stay quiet. [border_row] draws a separator between every pair
      of data rows.
    - [base_style] defaults to [Style.empty] and is the parent of every cell style and of
      [border_style], so a background set once covers the whole table. [border_style]
      defaults to [base_style] and styles the border glyphs and separators.
    - [style] defaults to [base_style] for every cell.
    - [width] leaves the columns at content width; with [fit_content] set, [width] becomes
      a maximum instead of a target to expand to.
    - [height] limits the rendered rows and [offset] skips rows before them; [offset] is
      clamped to zero or more.
    - [wrap] defaults to [true]: data cells that do not fit their column are wrapped.
      Headers are never wrapped, and [-1] is passed to [style] for the header row. *)

val render : t -> string
(** [render table] is the formatted table: the border lines the toggles allow, the header
    row, the visible data rows, an overflow row of […] when [height] cuts the data short,
    and the bottom border. *)

val data : t -> Data.t
(** [data table] is the row source. *)

val headers : t -> string list
(** [headers table] is the header row. *)

val height : t -> int
(** [height table] is the requested height, or [0] when the table is unconstrained. *)

val y_offset : t -> int
(** [y_offset table] is the number of data rows skipped before rendering. *)

val first_visible_row : t -> int
(** [first_visible_row table] is the index of the first data row {!val:render} emits,
    after the offset is clamped to the row count. *)

val last_visible_row : t -> int
(** [last_visible_row table] is the index of the last data row {!val:render} emits, or
    [-1] when the window reaches the final row and nothing is cut. *)

val visible_rows : t -> int
(** [visible_rows table] is the number of data rows {!val:render} emits. *)
