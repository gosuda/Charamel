(** Huh's immutable style bundle.

    A style separates visual lipgloss values from indicator text so fields can change
    palette without changing their state. *)

type text_input = {
  cursor : Charamel_lipgloss.Style.t;
  cursor_text : Charamel_lipgloss.Style.t;
  placeholder : Charamel_lipgloss.Style.t;
  prompt : Charamel_lipgloss.Style.t;
  text : Charamel_lipgloss.Style.t;
}

type indicators = {
  error_indicator : string;
  select_selector : string;
  next_indicator : string;
  prev_indicator : string;
  multi_select_selector : string;
  selected_prefix : string;
  unselected_prefix : string;
}

type field = {
  base : Charamel_lipgloss.Style.t;
  title : Charamel_lipgloss.Style.t;
  description : Charamel_lipgloss.Style.t;
  error_indicator : Charamel_lipgloss.Style.t;
  error_message : Charamel_lipgloss.Style.t;
  select_selector : Charamel_lipgloss.Style.t;
  option_ : Charamel_lipgloss.Style.t;
  next_indicator : Charamel_lipgloss.Style.t;
  prev_indicator : Charamel_lipgloss.Style.t;
  directory : Charamel_lipgloss.Style.t;
  file : Charamel_lipgloss.Style.t;
  multi_select_selector : Charamel_lipgloss.Style.t;
  selected_option : Charamel_lipgloss.Style.t;
  selected_prefix : Charamel_lipgloss.Style.t;
  unselected_option : Charamel_lipgloss.Style.t;
  unselected_prefix : Charamel_lipgloss.Style.t;
  focused_button : Charamel_lipgloss.Style.t;
  blurred_button : Charamel_lipgloss.Style.t;
  card : Charamel_lipgloss.Style.t;
  note_title : Charamel_lipgloss.Style.t;
  next : Charamel_lipgloss.Style.t;
  text_input : text_input;
  indicators : indicators;
}

type t = {
  form_base : Charamel_lipgloss.Style.t;
  group_base : Charamel_lipgloss.Style.t;
  group_title : Charamel_lipgloss.Style.t;
  group_description : Charamel_lipgloss.Style.t;
  field_separator : string;
  focused : field;
  blurred : field;
  help : Charamel_bubbles.Help.styles;
}

val base : is_dark:bool -> t
val charm : is_dark:bool -> t
val dracula : is_dark:bool -> t
val base16 : is_dark:bool -> t
val catppuccin : is_dark:bool -> t
