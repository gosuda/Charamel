(** Immutable text style values.

    A style combines terminal attributes with block geometry. Unset properties remain
    unset during inheritance; setters return a new value and never mutate their input. *)

type t

val empty : t
(** [empty] is a style with no attributes or geometry. *)

val bold : bool -> t -> t
val italic : bool -> t -> t
val underline : bool -> t -> t
val underline_style : Charm_ansi.Style.underline -> t -> t
val underline_color : Charm_ansi.Color.t -> t -> t
val strikethrough : bool -> t -> t
val reverse : bool -> t -> t
val blink : bool -> t -> t
val faint : bool -> t -> t
val underline_spaces : bool -> t -> t
val strikethrough_spaces : bool -> t -> t
val color_whitespace : bool -> t -> t
val foreground : Charm_ansi.Color.t -> t -> t
val background : Charm_ansi.Color.t -> t -> t
val width : int -> t -> t
val height : int -> t -> t
val max_width : int -> t -> t
val max_height : int -> t -> t
val align : Position.t -> t -> t
val align_horizontal : Position.t -> t -> t
val align_vertical : Position.t -> t -> t
val padding : Sides.t -> t -> t
val margin : Sides.t -> t -> t
val margin_background : Charm_ansi.Color.t -> t -> t
val border : Border.t -> t -> t
val border_top : bool -> t -> t
val border_right : bool -> t -> t
val border_bottom : bool -> t -> t
val border_left : bool -> t -> t
val border_foreground : Sides_color.t -> t -> t
val border_background : Sides_color.t -> t -> t
val inline : bool -> t -> t
val tab_width : int -> t -> t

val transform : (string -> string) -> t -> t
(** [transform f t] applies [f] to rendered text before layout. *)

val hyperlink : Charm_ansi.Link.t -> t -> t
(** [hyperlink link t] associates [link] with the rendered core text. *)

val unset_hyperlink : t -> t
(** [unset_hyperlink t] removes the hyperlink from [t]. *)

val get_hyperlink : t -> Charm_ansi.Link.t option
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
val unset_margin_background : t -> t
val unset_border : t -> t
val unset_border_top : t -> t
val unset_border_right : t -> t
val unset_border_bottom : t -> t
val unset_border_left : t -> t
val unset_border_foreground : t -> t
val unset_border_background : t -> t
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
val get_foreground : t -> Charm_ansi.Color.t option
val get_background : t -> Charm_ansi.Color.t option
val get_border : t -> Border.t option
val get_align_horizontal : t -> Position.t option
val get_align_vertical : t -> Position.t option
val get_tab_width : t -> int option
val get_bold : t -> bool option
val get_italic : t -> bool option
val get_underline : t -> bool option
val get_underline_style : t -> Charm_ansi.Style.underline option
val get_underline_color : t -> Charm_ansi.Color.t option
val get_strikethrough : t -> bool option
val get_reverse : t -> bool option
val get_blink : t -> bool option
val get_faint : t -> bool option
val get_underline_spaces : t -> bool option
val get_strikethrough_spaces : t -> bool option
val get_color_whitespace : t -> bool option
val get_margin_background : t -> Charm_ansi.Color.t option
val get_border_top : t -> bool option
val get_border_right : t -> bool option
val get_border_bottom : t -> bool option
val get_border_left : t -> bool option
val get_inline : t -> bool option

val get_transform : t -> (string -> string) option
(** [get_transform t] is the string transform of [t], or [None] when no transform is set.
*)
