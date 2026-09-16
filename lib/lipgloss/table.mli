(** Configurable text tables. *)

type t

val v :
  ?headers:string list ->
  ?rows:string list list ->
  ?border:Border.t ->
  ?style:(row:int -> col:int -> Style.t) ->
  ?width:int ->
  ?height:int ->
  ?offset:int ->
  ?wrap:bool ->
  unit ->
  t
(** [v ?headers ?rows ?border ?style ?width ?height ?offset ?wrap ()] is a table. Headers
    and rows default to empty lists. The border defaults to [Border.normal]. Cell styles
    default to [Style.empty]. Size is unconstrained, offset is [0], and wrapping is
    disabled by default. [style] receives [-1] for the header row and zero-based indexes
    for data rows. Tables are constructed through [v]. Mutable row setters, fit-content
    mode, and per-edge border toggles are outside this API. [render] uses the
    intersections in [Border.t] and its [left] glyph between cells. *)

val render : t -> string
(** [render table] is the formatted table. *)
