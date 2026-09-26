(** Immutable text style values.

    A style combines terminal attributes with block geometry. Unset properties remain
    unset during inheritance; setters return a new value and never mutate their input. *)

type t

val empty : t
(** [empty] is a style with no attributes or geometry. *)

val bold : bool -> t -> t
val italic : bool -> t -> t
val underline : bool -> t -> t
val underline_style : Charamel_ansi.Style.underline -> t -> t
val underline_color : Charamel_ansi.Color.t -> t -> t
val strikethrough : bool -> t -> t
val reverse : bool -> t -> t
val blink : bool -> t -> t
val faint : bool -> t -> t
val underline_spaces : bool -> t -> t
val strikethrough_spaces : bool -> t -> t
val color_whitespace : bool -> t -> t
val foreground : Charamel_ansi.Color.t -> t -> t
val background : Charamel_ansi.Color.t -> t -> t
val width : int -> t -> t
val height : int -> t -> t
val max_width : int -> t -> t
val max_height : int -> t -> t
val align : Position.t -> t -> t
val align_horizontal : Position.t -> t -> t
val align_vertical : Position.t -> t -> t

type side = [ `Top | `Right | `Bottom | `Left ]
(** The four sides of a block. *)

type sides = {
  top : int option;
  right : int option;
  bottom : int option;
  left : int option;
}
(** The type for per-side cell counts. Each side is set or unset on its own; an unset side
    contributes no cells and is not inherited. *)

val padding : Sides.t -> t -> t
(** [padding sides t] sets all four padding sides at once from a total {!Sides.t}. *)

val margin : Sides.t -> t -> t
(** [margin sides t] sets all four margin sides at once from a total {!Sides.t}. *)

val padding_side : side -> int -> t -> t
(** [padding_side side cells t] sets one padding side, leaving the other three as they
    are. *)

val margin_side : side -> int -> t -> t
(** [margin_side side cells t] sets one margin side, leaving the other three as they are.
*)

val padding_char : string -> t -> t
(** [padding_char text t] fills padding with [text], cycled by grapheme like the {!Layout}
    whitespace pattern. [""] restores the plain space.
    @raise Invalid_argument if [text] is more than one grapheme. *)

val margin_char : string -> t -> t
(** [margin_char text t] fills margins with [text] under the same rules as
    {!val:padding_char}, styled by the margin background. *)

val margin_background : Charamel_ansi.Color.t -> t -> t
val border : Border.t -> t -> t
val border_top : bool -> t -> t
val border_right : bool -> t -> t
val border_bottom : bool -> t -> t
val border_left : bool -> t -> t
val border_foreground : Sides_color.t -> t -> t
val border_background : Sides_color.t -> t -> t

val border_foreground_blend : Charamel_ansi.Color.t list -> t -> t
(** [border_foreground_blend colors t] paints every border glyph from a gradient blended
    through CIE L*a*b* around the frame perimeter, in place of the per-edge foreground
    colors. An empty [colors] leaves the plain foreground in place. *)

val border_foreground_blend_offset : int -> t -> t
(** [border_foreground_blend_offset cells t] rotates the border gradient forward by
    [cells] steps from the top-left corner. *)

val inline : bool -> t -> t

val tab_width : int -> t -> t
(** [tab_width cells t] replaces every tab of the rendered text with [cells] spaces. [0]
    deletes tabs, {!val:no_tab_conversion} keeps them, and the default when unset is 4. *)

val transform : (string -> string) -> t -> t
(** [transform f t] applies [f] to rendered text before layout. *)

val hyperlink : Charamel_ansi.Link.t -> t -> t
(** [hyperlink link t] associates [link] with the rendered core text. *)

val unset_hyperlink : t -> t
(** [unset_hyperlink t] removes the hyperlink from [t]. *)

val get_hyperlink : t -> Charamel_ansi.Link.t option
(** [get_hyperlink t] is the hyperlink of [t], or [None] when unset. *)

val get_border_foreground : t -> Sides_color.t option
(** [get_border_foreground t] is the per-edge border foreground colors of [t]. *)

val get_border_background : t -> Sides_color.t option
(** [get_border_background t] is the per-edge border background colors of [t]. *)

val unset_bold : t -> t
val unset_italic : t -> t
val unset_underline : t -> t
val unset_underline_style : t -> t
val unset_underline_color : t -> t
val unset_strikethrough : t -> t
val unset_reverse : t -> t
val unset_blink : t -> t
val unset_faint : t -> t
val unset_underline_spaces : t -> t
val unset_strikethrough_spaces : t -> t
val unset_color_whitespace : t -> t
val unset_foreground : t -> t
val unset_background : t -> t
val unset_width : t -> t
val unset_height : t -> t
val unset_max_width : t -> t
val unset_max_height : t -> t
val unset_align : t -> t
val unset_align_horizontal : t -> t
val unset_align_vertical : t -> t
val unset_padding : t -> t
val unset_margin : t -> t
val unset_padding_side : side -> t -> t
val unset_margin_side : side -> t -> t
val unset_padding_char : t -> t
val unset_margin_char : t -> t
val unset_margin_background : t -> t
val unset_border : t -> t
val unset_border_top : t -> t
val unset_border_right : t -> t
val unset_border_bottom : t -> t
val unset_border_left : t -> t
val unset_border_foreground : t -> t
val unset_border_background : t -> t
val unset_border_foreground_blend : t -> t
val unset_border_foreground_blend_offset : t -> t
val unset_inline : t -> t
val unset_tab_width : t -> t
val unset_transform : t -> t

val inherit_ : parent:t -> t -> t
(** [inherit_ ~parent child] fills unset properties in [child] from [parent]. Padding,
    margins, and hyperlinks are never inherited. A parent background supplies a child's
    margin background when neither side sets one. *)

val render : t -> string -> string
(** [render t text] is [text] transformed, wrapped, styled, aligned, and framed according
    to [t]. Width excludes margins and height is a minimum. *)

val get_width : t -> int option
val get_height : t -> int option
val get_max_width : t -> int option
val get_max_height : t -> int option
val get_padding : t -> Sides.t option

val get_margin : t -> Sides.t option
(** [get_padding t] is the total padding record of [t], with unset sides as [0], or [None]
    when no side is set. *)

val get_padding_side : side -> t -> int option

val get_margin_side : side -> t -> int option
(** [get_padding_side side t] is the cells set on that one padding side, or [None]. *)

val get_padding_char : t -> string option

val get_margin_char : t -> string option
(** [get_padding_char t] is the padding fill character of [t], or [None] for a plain
    space. *)

val get_foreground : t -> Charamel_ansi.Color.t option
val get_background : t -> Charamel_ansi.Color.t option
val get_border : t -> Border.t option
val get_align_horizontal : t -> Position.t option
val get_align_vertical : t -> Position.t option
val get_tab_width : t -> int option
val get_bold : t -> bool option
val get_italic : t -> bool option
val get_underline : t -> bool option
val get_underline_style : t -> Charamel_ansi.Style.underline option
val get_underline_color : t -> Charamel_ansi.Color.t option
val get_strikethrough : t -> bool option
val get_reverse : t -> bool option
val get_blink : t -> bool option
val get_faint : t -> bool option
val get_underline_spaces : t -> bool option
val get_strikethrough_spaces : t -> bool option
val get_color_whitespace : t -> bool option
val get_margin_background : t -> Charamel_ansi.Color.t option
val get_border_top : t -> bool option
val get_border_right : t -> bool option
val get_border_bottom : t -> bool option
val get_border_left : t -> bool option
val get_border_foreground_blend : t -> Charamel_ansi.Color.t list option
val get_border_foreground_blend_offset : t -> int option
val get_inline : t -> bool option

val get_align : t -> Position.t option
(** [get_align t] is the horizontal alignment of [t]. It is the alias of
    {!val:get_align_horizontal} that upstream's [GetAlign] carries. *)

val get_border_top_size : t -> int
val get_border_right_size : t -> int
val get_border_bottom_size : t -> int

val get_border_left_size : t -> int
(** [get_border_top_size t] is the cell width the named border edge occupies: [0] when the
    edge is off or no border is set, and otherwise the width of the widest glyph among
    that edge and its two corners. *)

val get_horizontal_border_size : t -> int

val get_vertical_border_size : t -> int
(** [get_horizontal_border_size t] is the sum of the left and right border sizes, and
    [get_vertical_border_size t] the sum of the top and bottom ones. *)

val get_horizontal_padding : t -> int
val get_vertical_padding : t -> int
val get_horizontal_margins : t -> int

val get_vertical_margins : t -> int
(** [get_horizontal_padding t] is the sum of the two horizontal sides of [t]'s padding,
    and likewise for the other three. An unset property contributes [0]. *)

val get_horizontal_frame_size : t -> int
val get_vertical_frame_size : t -> int

val get_frame_size : t -> int * int
(** [get_frame_size t] is [(get_horizontal_frame_size t, get_vertical_frame_size t)]: the
    cells each axis spends on padding, margins, and border. *)

val no_tab_conversion : int
(** [no_tab_conversion] is the {!val:tab_width} setting that leaves tabs alone: [-1]. *)

val nbsp : string
(** [nbsp] is the non-breaking space, ["\u{00A0}"], the character upstream recommends for
    padding that must survive copy and paste. *)

val get_transform : t -> (string -> string) option
(** [get_transform t] is the string transform of [t], or [None] when no transform is set.
*)
