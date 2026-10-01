(** Composition of a layer tree onto a canvas, with hit testing.

    A compositor flattens its layer tree once: every layer keeps the absolute position it
    gets by accumulating its ancestors' offsets, and the layers are ordered by [z].
    Drawing goes from the lowest [z] to the highest, so a later layer covers an earlier
    one; hit testing searches the other way, so the topmost layer answers first. Layers
    with an empty identifier are drawn but never hit and never indexed. *)

type bounds = { x0 : int; y0 : int; x1 : int; y1 : int }
(** The type for half-open rectangles: [x1] and [y1] are one past the last occupied column
    and row, so an empty rectangle has equal pairs. *)

type hit = { id : string; layer : Layer.t; bounds : bounds }
(** The type for a hit: the layer that answered, its identifier and its absolute
    rectangle. *)

type t
(** The type for compositors. *)

val v : Layer.t list -> t
(** [v layers] is the compositor of those layers, under an implicit root at the origin. *)

val add : t -> Layer.t list -> t
(** [add compositor layers] appends [layers] to the root and rebuilds the composition. *)

val bounds : t -> bounds option
(** [bounds compositor] is the union of every flattened layer's rectangle, including the
    implicit root at the origin, or [None] when the composition is empty. *)

val hit : t -> x:int -> y:int -> hit option
(** [hit compositor ~x ~y] is the topmost layer whose rectangle contains the point. [None]
    when no identified layer covers it. *)

val find : t -> string -> Layer.t option
(** [find compositor id] is the layer of that identifier, or [None]. *)

val refresh : t -> t
(** [refresh compositor] is [compositor]. A compositor is an immutable value, so it is
    rebuilt by {!val:v} and {!val:add} rather than refreshed; this is the seam a caller
    carrying a mutable layer tree would use. *)

val render : t -> string
(** [render compositor] draws every layer onto a canvas sized to {!val:bounds}, moved so
    the bounds' top-left corner lands at the canvas origin, and renders that canvas with
    trailing spaces trimmed. *)
