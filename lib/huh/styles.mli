(** Huh's immutable style bundle.

    A style separates visual lipgloss values from indicator text so fields can change
    palette without changing their state. *)

type text_input = {
  cursor : Charm_lipgloss.Style.t;
  cursor_text : Charm_lipgloss.Style.t;
  placeholder : Charm_lipgloss.Style.t;
  prompt : Charm_lipgloss.Style.t;
  text : Charm_lipgloss.Style.t;
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
  base : Charm_lipgloss.Style.t;
  title : Charm_lipgloss.Style.t;
  description : Charm_lipgloss.Style.t;
  error_indicator : Charm_lipgloss.Style.t;
  error_message : Charm_lipgloss.Style.t;
  select_selector : Charm_lipgloss.Style.t;
  option_ : Charm_lipgloss.Style.t;
  next_indicator : Charm_lipgloss.Style.t;
  prev_indicator : Charm_lipgloss.Style.t;
  directory : Charm_lipgloss.Style.t;
  file : Charm_lipgloss.Style.t;
  multi_select_selector : Charm_lipgloss.Style.t;
  selected_option : Charm_lipgloss.Style.t;
  selected_prefix : Charm_lipgloss.Style.t;
  unselected_option : Charm_lipgloss.Style.t;
  unselected_prefix : Charm_lipgloss.Style.t;
  focused_button : Charm_lipgloss.Style.t;
  blurred_button : Charm_lipgloss.Style.t;
  card : Charm_lipgloss.Style.t;
  note_title : Charm_lipgloss.Style.t;
  next : Charm_lipgloss.Style.t;
  text_input : text_input;
  indicators : indicators;
}

type t = {
  form_base : Charm_lipgloss.Style.t;
  group_base : Charm_lipgloss.Style.t;
  group_title : Charm_lipgloss.Style.t;
  group_description : Charm_lipgloss.Style.t;
  field_separator : string;
  focused : field;
  blurred : field;
  help : Charm_bubbles.Help.styles;
}

val base : is_dark:bool -> t
val charm : is_dark:bool -> t
val dracula : is_dark:bool -> t
val base16 : is_dark:bool -> t
val catppuccin : is_dark:bool -> t
