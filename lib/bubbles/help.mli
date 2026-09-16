(** Pure rendering of short and full keyboard-help views. *)

type styles = {
  ellipsis : Charm_lipgloss.Style.t;
  short_key : Charm_lipgloss.Style.t;
  short_desc : Charm_lipgloss.Style.t;
  short_separator : Charm_lipgloss.Style.t;
  full_key : Charm_lipgloss.Style.t;
  full_desc : Charm_lipgloss.Style.t;
  full_separator : Charm_lipgloss.Style.t;
}

val default_styles : is_dark:bool -> styles

type keymap = { short_help : Key_binding.t list; full_help : Key_binding.t list list }
type t

val v :
  ?width:int ->
  ?show_all:bool ->
  ?short_separator:string ->
  ?full_separator:string ->
  ?ellipsis:string ->
  ?is_dark:bool ->
  ?styles:styles ->
  unit ->
  t

val view : t -> keymap -> string
val short_view : t -> Key_binding.t list -> string
val full_view : t -> Key_binding.t list list -> string
val width : t -> int
val set_width : int -> t -> t
val show_all : t -> bool
val set_show_all : bool -> t -> t
val set_styles : styles -> t -> t
val styles : t -> styles
