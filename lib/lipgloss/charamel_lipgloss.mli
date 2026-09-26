(** Terminal layout and styling primitives.

    Lipgloss values are immutable and render through the full-fidelity ANSI primitives. *)

module Position = Position
module Sides = Sides
module Color = Charamel_ansi.Color
module Color_util = Color_util
module Blending = Blending
module Print = Print

(** The underline kinds a style can request, re-exported so a caller of the aggregator
    does not need a direct {!charamel.ansi} dependency. *)
module Underline : sig
  type t = Charamel_ansi.Style.underline =
    | No_underline
    | Single
    | Double
    | Curly
    | Dotted
    | Dashed
end

module Sides_color = Sides_color
module Border = Border

val light_dark : is_dark:bool -> light:Color.t -> dark:Color.t -> Color.t
(** [light_dark ~is_dark ~light ~dark] is [dark] when [is_dark] is [true] and [light]
    otherwise. *)

module Style = Style
module Layout = Layout
module Table = Table
module Tree = Tree
module List = List
