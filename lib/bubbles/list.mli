(** Filterable, paginated list component. *)

type filter_state = Unfiltered | Filtering | Filter_applied
type rank = { index : int; matched : int list }
type filter = string -> string list -> rank list

val default_filter : filter
val unsorted_filter : filter

type keymap = {
  cursor_up : Key_binding.t;
  cursor_down : Key_binding.t;
  next_page : Key_binding.t;
  prev_page : Key_binding.t;
  go_to_start : Key_binding.t;
  go_to_end : Key_binding.t;
  filter : Key_binding.t;
  clear_filter : Key_binding.t;
  cancel_while_filtering : Key_binding.t;
  accept_while_filtering : Key_binding.t;
  show_full_help : Key_binding.t;
  close_full_help : Key_binding.t;
  quit : Key_binding.t;
  force_quit : Key_binding.t;
}

val default_keymap : keymap

type styles = {
  title_bar : Charm_lipgloss.Style.t;
  title : Charm_lipgloss.Style.t;
  spinner : Charm_lipgloss.Style.t;
  filter_prompt : Charm_lipgloss.Style.t;
  filter_cursor : Charm_lipgloss.Style.t;
  default_filter_character_match : Charm_lipgloss.Style.t;
  status_bar : Charm_lipgloss.Style.t;
  status_empty : Charm_lipgloss.Style.t;
  status_bar_active_filter : Charm_lipgloss.Style.t;
  status_bar_filter_count : Charm_lipgloss.Style.t;
  no_items : Charm_lipgloss.Style.t;
  pagination_style : Charm_lipgloss.Style.t;
  help_style : Charm_lipgloss.Style.t;
  active_pagination_dot : Charm_lipgloss.Style.t;
  inactive_pagination_dot : Charm_lipgloss.Style.t;
  arabic_pagination : Charm_lipgloss.Style.t;
  divider_dot : Charm_lipgloss.Style.t;
}

val default_styles : is_dark:bool -> styles

type 'a item_context = {
  index : int;
  selected : bool;
  filter_state : filter_state;
  filter_text : string;
  matched : int list;
  width : int;
}

type 'a delegate = {
  height : int;
  spacing : int;
  render : 'a item_context -> 'a -> string;
  short_help : Key_binding.t list;
  full_help : Key_binding.t list list;
}

type item_styles = {
  normal_title : Charm_lipgloss.Style.t;
  normal_desc : Charm_lipgloss.Style.t;
  selected_title : Charm_lipgloss.Style.t;
  selected_desc : Charm_lipgloss.Style.t;
  dimmed_title : Charm_lipgloss.Style.t;
  dimmed_desc : Charm_lipgloss.Style.t;
  filter_match : Charm_lipgloss.Style.t;
}

val default_item_styles : is_dark:bool -> item_styles

val default_delegate :
  ?show_description:bool ->
  ?height:int ->
  ?spacing:int ->
  ?styles:item_styles ->
  ?is_dark:bool ->
  title:('a -> string) ->
  ?description:('a -> string) ->
  unit ->
  'a delegate

type 'a msg =
  | Cursor_up
  | Cursor_down
  | Next_page
  | Prev_page
  | Go_to_start
  | Go_to_end
  | Start_filter
  | Clear_filter
  | Cancel_filter
  | Accept_filter
  | Toggle_full_help
  | Filter_input of Textinput.msg
  | Status_timeout of int
  | Spinner of Spinner.msg
  | Set_items of 'a list

type 'a t

val v :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?keymap:keymap ->
  ?is_dark:bool ->
  ?styles:styles ->
  ?filter:filter ->
  ?filtering_enabled:bool ->
  ?show_title:bool ->
  ?show_filter:bool ->
  ?show_status_bar:bool ->
  ?show_pagination:bool ->
  ?show_help:bool ->
  ?infinite_scrolling:bool ->
  ?status_message_lifetime:float ->
  ?item_name:string * string ->
  delegate:'a delegate ->
  filter_value:('a -> string) ->
  'a list ->
  'a t

val update : 'a msg -> 'a t -> 'a t * 'a msg Charm_tea.Cmd.t
val view : 'a t -> string
val key : 'a t -> Charm_tea.Key.t -> 'a msg option
val subscriptions : 'a t -> 'a msg Charm_tea.Sub.t
val items : 'a t -> 'a list
val set_items : 'a list -> 'a t -> 'a t
val visible_items : 'a t -> 'a list
val selected_item : 'a t -> 'a option
val index : 'a t -> int
val select : int -> 'a t -> 'a t
val reset_selected : 'a t -> 'a t
val global_index : 'a t -> int
val insert_item : int -> 'a -> 'a t -> 'a t
val remove_item : int -> 'a t -> 'a t
val set_item : int -> 'a -> 'a t -> 'a t
val cursor_up : 'a t -> 'a t
val cursor_down : 'a t -> 'a t
val next_page : 'a t -> 'a t
val prev_page : 'a t -> 'a t
val go_to_start : 'a t -> 'a t
val go_to_end : 'a t -> 'a t
val filter_state : 'a t -> filter_state
val set_filter_state : filter_state -> 'a t -> 'a t
val filter_value : 'a t -> string
val set_filter_text : string -> 'a t -> 'a t
val reset_filter : 'a t -> 'a t
val setting_filter : 'a t -> bool
val is_filtered : 'a t -> bool
val filtering_enabled : 'a t -> bool
val set_filtering_enabled : bool -> 'a t -> 'a t
val width : 'a t -> int
val height : 'a t -> int
val set_size : width:int -> height:int -> 'a t -> 'a t
val set_width : int -> 'a t -> 'a t
val set_height : int -> 'a t -> 'a t
val title : 'a t -> string
val set_title : string -> 'a t -> 'a t
val show_title : 'a t -> bool
val set_show_title : bool -> 'a t -> 'a t
val show_filter : 'a t -> bool
val set_show_filter : bool -> 'a t -> 'a t
val show_status_bar : 'a t -> bool
val set_show_status_bar : bool -> 'a t -> 'a t
val show_pagination : 'a t -> bool
val set_show_pagination : bool -> 'a t -> 'a t
val show_help : 'a t -> bool
val set_show_help : bool -> 'a t -> 'a t
val set_status_bar_item_name : string -> string -> 'a t -> 'a t
val new_status_message : string -> 'a t -> 'a t * 'a msg Charm_tea.Cmd.t
val start_spinner : 'a t -> 'a t
val stop_spinner : 'a t -> 'a t
val toggle_spinner : 'a t -> 'a t
val set_spinner : Spinner.kind -> 'a t -> 'a t
val set_delegate : 'a delegate -> 'a t -> 'a t
val disable_quit_keybindings : 'a t -> 'a t
val set_additional_short_help_keys : Key_binding.t list -> 'a t -> 'a t
val set_additional_full_help_keys : Key_binding.t list -> 'a t -> 'a t
val short_help : 'a t -> Key_binding.t list
val full_help : 'a t -> Key_binding.t list list
val paginator : 'a t -> Paginator.t
val keymap : 'a t -> keymap
val styles : 'a t -> styles
val set_styles : styles -> 'a t -> 'a t
val infinite_scrolling : 'a t -> bool
val set_infinite_scrolling : bool -> 'a t -> 'a t
