(** Scrollable terminal viewport with optional soft wrapping, gutters, and highlights. *)

type keymap = {
  page_down : Key_binding.t;
  page_up : Key_binding.t;
  half_page_up : Key_binding.t;
  half_page_down : Key_binding.t;
  up : Key_binding.t;
  down : Key_binding.t;
  left : Key_binding.t;
  right : Key_binding.t;
}

val default_keymap : keymap

type gutter_context = { index : int; total_lines : int; soft : bool }

type msg =
  | Page_down
  | Page_up
  | Half_page_up
  | Half_page_down
  | Up
  | Down
  | Left
  | Right
  | Wheel of Charamel_tea.Mouse.t

type t

val v :
  ?width:int ->
  ?height:int ->
  ?keymap:keymap ->
  ?style:Charamel_lipgloss.Style.t ->
  ?soft_wrap:bool ->
  ?fill_height:bool ->
  ?mouse_wheel_enabled:bool ->
  ?mouse_wheel_delta:int ->
  ?horizontal_step:int ->
  ?left_gutter:(gutter_context -> string) ->
  ?style_line:(int -> Charamel_lipgloss.Style.t) ->
  ?highlight_style:Charamel_lipgloss.Style.t ->
  ?selected_highlight_style:Charamel_lipgloss.Style.t ->
  unit ->
  t

val update : msg -> t -> t * msg Charamel_tea.Cmd.t
val view : t -> string
val key : t -> Charamel_tea.Key.t -> msg option
val mouse : t -> Charamel_tea.Mouse.t -> msg option
val subscriptions : t -> msg Charamel_tea.Sub.t
val set_content : string -> t -> t
val set_content_lines : string list -> t -> t
val content : t -> string
val width : t -> int
val set_width : int -> t -> t
val height : t -> int
val set_height : int -> t -> t
val y_offset : t -> int
val set_y_offset : int -> t -> t
val x_offset : t -> int
val set_x_offset : int -> t -> t
val style : t -> Charamel_lipgloss.Style.t
val set_style : Charamel_lipgloss.Style.t -> t -> t
val soft_wrap : t -> bool
val set_soft_wrap : bool -> t -> t
val set_fill_height : bool -> t -> t
val set_left_gutter : (gutter_context -> string) option -> t -> t
val set_style_line : (int -> Charamel_lipgloss.Style.t) option -> t -> t
val set_horizontal_step : int -> t -> t
val set_mouse_wheel_enabled : bool -> t -> t
val set_mouse_wheel_delta : int -> t -> t
val at_top : t -> bool
val at_bottom : t -> bool
val past_bottom : t -> bool
val scroll_percent : t -> float
val horizontal_scroll_percent : t -> float
val goto_top : t -> t
val goto_bottom : t -> t
val page_down : t -> t
val page_up : t -> t
val half_page_down : t -> t
val half_page_up : t -> t
val scroll_down : int -> t -> t
val scroll_up : int -> t -> t
val scroll_left : int -> t -> t
val scroll_right : int -> t -> t
val ensure_visible : line:int -> colstart:int -> colend:int -> t -> t
val total_line_count : t -> int
val visible_line_count : t -> int
val visible_lines : t -> string list

val set_highlights : (int * int) list -> t -> t
(** [set_highlights ranges viewport] marks [ranges], half-open grapheme-cluster index
    pairs over the whole content in [Charamel_ansi.Width.graphemes] order (newlines count
    as one cluster). The first range at or below the scroll position is selected and
    scrolled into view. Empty input clears the marks. *)

val clear_highlights : t -> t
val highlight_next : t -> t
val highlight_previous : t -> t
val set_highlight_style : Charamel_lipgloss.Style.t -> t -> t
val set_selected_highlight_style : Charamel_lipgloss.Style.t -> t -> t

val grapheme_ranges_of_byte_ranges : t -> (int * int) list -> (int * int) list
(** [grapheme_ranges_of_byte_ranges viewport ranges] converts half-open byte-offset
    ranges, as produced by substring searches over [content viewport], into the
    grapheme-index ranges expected by [set_highlights]. The offsets are interpreted
    against the viewport's CRLF-normalized content, the same coordinate space
    [set_highlights] uses. A byte range touching any part of a cluster marks the whole
    cluster. *)
