(** A mutable-in-value grid of cells that layers can be drawn onto.

    A canvas holds {!Charamel_ansi.Raster.cell}s addressed by column and row. Every
    operation returns a new canvas; the argument is never changed. Drawing text rasterises
    it first, so the styles and hyperlinks it carries land in the cells exactly as
    {!Charamel_ansi.Raster} describes them. *)

type t
(** The type for canvases. *)

val create : width:int -> height:int -> t
(** [create ~width ~height] is a canvas of blank cells. A width or height of zero or less
    yields a canvas that holds nothing and accepts no drawing. *)

val resize : t -> width:int -> height:int -> t
(** [resize canvas ~width ~height] is a canvas of the requested size holding the cells of
    [canvas] that still fit: content is kept from the top-left corner, and new cells are
    blank. *)

val width : t -> int
(** [width canvas] is the number of columns. *)

val height : t -> int
(** [height canvas] is the number of rows. *)

val clear : t -> t
(** [clear canvas] is a canvas of the same size with every cell blank. *)

val cell_at : t -> x:int -> y:int -> Charamel_ansi.Raster.cell
(** [cell_at canvas ~x ~y] is the cell at that position, or {!Charamel_ansi.Raster.blank}
    when the position is outside the canvas. *)

val set_cell : t -> x:int -> y:int -> Charamel_ansi.Raster.cell -> t
(** [set_cell canvas ~x ~y cell] puts [cell] at that position. A position outside the
    canvas is ignored. *)

val draw : t -> x:int -> y:int -> string -> t
(** [draw canvas ~x ~y text] rasterises [text] into the region of the canvas at and beyond
    [(x, y)] and writes the cells the text occupies there, leaving every other cell of the
    canvas untouched, so text that does not fit the region wraps within it and later
    drawing overwrites earlier drawing only where it paints. Empty text draws nothing. *)

val render : t -> string
(** [render canvas] is the terminal output that paints the canvas, with the trailing
    spaces of each line trimmed. *)
