(** Focusable table with fixed-width columns and viewport scrolling. *)

type column = { title : string; width : int }
type row = string list

type keymap = {
  line_up : Key_binding.t;
  line_down : Key_binding.t;
  page_up : Key_binding.t;
  page_down : Key_binding.t;
  half_page_up : Key_binding.t;
  half_page_down : Key_binding.t;
  goto_top : Key_binding.t;
  goto_bottom : Key_binding.t;
}

val default_keymap : keymap

type styles = {
  header : Charamel_lipgloss.Style.t;
  cell : Charamel_lipgloss.Style.t;
  selected : Charamel_lipgloss.Style.t;
}

val default_styles : styles

type msg =
  | Line_up
  | Line_down
  | Page_up
  | Page_down
  | Half_page_up
  | Half_page_down
  | Goto_top
  | Goto_bottom

type t

val v :
  ?columns:column list ->
  ?rows:row list ->
  ?height:int ->
  ?width:int ->
  ?focused:bool ->
  ?styles:styles ->
  ?keymap:keymap ->
  unit ->
  t

val update : msg -> t -> t * msg Charamel_tea.Cmd.t
val view : t -> string
val key : t -> Charamel_tea.Key.t -> msg option
val subscriptions : t -> msg Charamel_tea.Sub.t
val focus : t -> t
val blur : t -> t
val focused : t -> bool
val rows : t -> row list
val set_rows : row list -> t -> t
val columns : t -> column list
val set_columns : column list -> t -> t
val selected_row : t -> row option
val cursor : t -> int
val set_cursor : int -> t -> t
val move_up : int -> t -> t
val move_down : int -> t -> t
val goto_top : t -> t
val goto_bottom : t -> t
val width : t -> int
val set_width : int -> t -> t
val height : t -> int
val set_height : int -> t -> t
val help_view : t -> string
val short_help : t -> Key_binding.t list
val full_help : t -> Key_binding.t list list
val set_styles : styles -> t -> t
val styles : t -> styles
val of_values : ?separator:string -> string -> row list
