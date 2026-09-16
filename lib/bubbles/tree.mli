(** Expandable tree browser.

    [node] values are immutable. Open/closed state belongs to the picker model and is
    preserved by path, so every update returns a new model. *)

type node

val node : ?open_:bool -> ?value:string -> node list -> node
(** [node ?open_ ?value children] creates a branch. [open_] defaults to [true] and [value]
    defaults to the empty string. *)

val leaf : string -> node
(** [leaf value] creates a closed node with no children. *)

val value : node -> string
val children : node -> node list
val is_open : node -> bool

val size : node -> int
(** [size n] counts [n] and all visible descendants; a closed branch counts as one node.
*)

type keymap = {
  down : Key_binding.t;
  up : Key_binding.t;
  page_down : Key_binding.t;
  page_up : Key_binding.t;
  half_page_down : Key_binding.t;
  half_page_up : Key_binding.t;
  go_to_top : Key_binding.t;
  go_to_bottom : Key_binding.t;
  toggle : Key_binding.t;
  open_ : Key_binding.t;
  close : Key_binding.t;
  show_full_help : Key_binding.t;
  close_full_help : Key_binding.t;
}

val default_keymap : keymap
(** [default_keymap] is the standard tree navigation and expansion map. *)

type styles = {
  tree_style : Charm_lipgloss.Style.t;
  help_style : Charm_lipgloss.Style.t;
  node_style : Charm_lipgloss.Style.t;
  selected_node_style : Charm_lipgloss.Style.t;
  root_node_style : Charm_lipgloss.Style.t;
  parent_node_style : Charm_lipgloss.Style.t;
  cursor_style : Charm_lipgloss.Style.t;
  enumerator_style : Charm_lipgloss.Style.t;
  selected_enumerator_style : Charm_lipgloss.Style.t;
  indenter_style : Charm_lipgloss.Style.t;
  open_indicator_style : Charm_lipgloss.Style.t;
}

val default_styles : is_dark:bool -> styles
(** [default_styles ~is_dark] is the default tree palette. *)

type msg =
  | Down
  | Up
  | Page_down
  | Page_up
  | Half_page_down
  | Half_page_up
  | Go_to_top
  | Go_to_bottom
  | Toggle
  | Open
  | Close
  | Toggle_full_help

type t

val v :
  ?open_character:string ->
  ?closed_character:string ->
  ?cursor_character:string ->
  ?scroll_off:int ->
  ?show_help:bool ->
  ?keymap:keymap ->
  ?is_dark:bool ->
  ?styles:styles ->
  width:int ->
  height:int ->
  node ->
  t
(** [v ~width ~height root] creates a tree with the standard indicators ["▼"], ["▶"],
    cursor ["→"], scroll-off [5], and help enabled. *)

val update : msg -> t -> t * msg Charm_tea.Cmd.t
(** [update msg t] applies one navigation, expansion, or help action. *)

val view : t -> string
(** [view t] renders the visible tree and optional help footer. *)

val key : t -> Charm_tea.Key.t -> msg option
(** [key t key] returns the enabled action bound to [key]. *)

val subscriptions : t -> msg Charm_tea.Sub.t
(** [subscriptions t] is {!Charm_tea.Sub.none}. *)

val set_root : node -> t -> t
val root : t -> node
val y_offset : t -> int
val set_y_offset : int -> t -> t
val node_at_current_offset : t -> node option
val all_nodes : t -> node list
val down : t -> t
val up : t -> t
val page_down : t -> t
val page_up : t -> t
val half_page_down : t -> t
val half_page_up : t -> t
val go_to_top : t -> t
val go_to_bottom : t -> t
val toggle_current_node : t -> t
val open_current_node : t -> t
val close_current_node : t -> t
val set_size : width:int -> height:int -> t -> t
val set_width : int -> t -> t
val set_height : int -> t -> t
val width : t -> int
val height : t -> int
val set_open_character : string -> t -> t
val set_closed_character : string -> t -> t
val set_cursor_character : string -> t -> t
val set_scroll_off : int -> t -> t
val set_show_help : bool -> t -> t
val set_styles : styles -> t -> t
val set_additional_short_help_keys : Key_binding.t list -> t -> t
val set_additional_full_help_keys : Key_binding.t list -> t -> t
val short_help : t -> Key_binding.t list
val full_help : t -> Key_binding.t list list
