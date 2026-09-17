module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color

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

let hex value =
  match Color.of_hex value with
  | Some color -> color
  | None -> invalid_arg ("invalid Huh theme color: " ^ value)

let ld ~is_dark light dark = if is_dark then dark else light

let button =
  Style.empty
  |> Style.padding (Charamel_lipgloss.Sides.v ~right:2 ~left:2 ())
  |> Style.margin (Charamel_lipgloss.Sides.v ~right:1 ())

let no_style = Style.empty

let focus_base =
  no_style
  |> Style.padding (Charamel_lipgloss.Sides.v ~left:1 ())
  |> Style.border Charamel_lipgloss.Border.thick
  |> Style.border_top false |> Style.border_right false |> Style.border_bottom false

let base_field () =
  let text_input =
    {
      cursor = no_style;
      cursor_text = no_style;
      placeholder = Style.foreground (Color.Basic 8) no_style;
      prompt = no_style;
      text = no_style;
    }
  in
  {
    base = no_style;
    title = no_style;
    description = no_style;
    error_indicator = no_style;
    error_message = no_style;
    select_selector = no_style;
    option_ = no_style;
    next_indicator = no_style;
    prev_indicator = no_style;
    directory = no_style;
    file = no_style;
    multi_select_selector = no_style;
    selected_option = no_style;
    selected_prefix = no_style;
    unselected_option = no_style;
    unselected_prefix = no_style;
    focused_button =
      button |> Style.foreground (Color.Basic 0) |> Style.background (Color.Basic 7);
    blurred_button =
      button |> Style.foreground (Color.Basic 7) |> Style.background (Color.Basic 0);
    card = no_style;
    note_title = no_style;
    next = no_style;
    text_input;
    indicators =
      {
        error_indicator = " *";
        select_selector = "> ";
        next_indicator = "->";
        prev_indicator = "<-";
        multi_select_selector = "> ";
        selected_prefix = "[*] ";
        unselected_prefix = "[ ] ";
      };
  }

let blurred_from_focused focused =
  let base =
    focused.base
    |> Style.border Charamel_lipgloss.Border.hidden
    |> Style.border_top false |> Style.border_right false |> Style.border_bottom false
  in
  {
    focused with
    base;
    card = base;
    next_indicator = no_style;
    prev_indicator = no_style;
    multi_select_selector = focused.multi_select_selector;
    indicators =
      {
        focused.indicators with
        next_indicator = "";
        prev_indicator = "";
        multi_select_selector = "  ";
      };
  }

let base_help ~is_dark = Charamel_bubbles.Help.default_styles ~is_dark

let base ~is_dark:_ =
  let initial = base_field () in
  let focused = { initial with base = focus_base; card = focus_base } in
  {
    form_base = no_style;
    group_base = no_style;
    group_title = no_style;
    group_description = no_style;
    field_separator = "\n\n";
    focused;
    blurred = blurred_from_focused focused;
    help = base_help ~is_dark:true;
  }

let set_fields focused ~group_title ~group_description =
  ( {
      base = focused.base;
      title = focused.title;
      description = focused.description;
      error_indicator = focused.error_indicator;
      error_message = focused.error_message;
      select_selector = focused.select_selector;
      option_ = focused.option_;
      next_indicator = focused.next_indicator;
      prev_indicator = focused.prev_indicator;
      directory = focused.directory;
      file = focused.file;
      multi_select_selector = focused.multi_select_selector;
      selected_option = focused.selected_option;
      selected_prefix = focused.selected_prefix;
      unselected_option = focused.unselected_option;
      unselected_prefix = focused.unselected_prefix;
      focused_button = focused.focused_button;
      blurred_button = focused.blurred_button;
      card = focused.card;
      note_title = focused.note_title;
      next = focused.next;
      text_input = focused.text_input;
      indicators = focused.indicators;
    },
    group_title,
    group_description )

let charm ~is_dark =
  let normal_fg = ld ~is_dark (Color.Indexed 252) (Color.Indexed 235) in
  let indigo = ld ~is_dark (hex "#5A56E0") (hex "#7571F9") in
  let cream = hex "#FFFDF5" in
  let fuchsia = hex "#F780E2" in
  let green = ld ~is_dark (hex "#02BA84") (hex "#02BF87") in
  let red = ld ~is_dark (hex "#FF4672") (hex "#ED567A") in
  let focused0 = { (base_field ()) with base = focus_base; card = focus_base } in
  let focused =
    {
      focused0 with
      base =
        focused0.base
        |> Style.border Charamel_lipgloss.Border.thick
        |> Style.border_top false |> Style.border_right false |> Style.border_bottom false
        |> Style.border_foreground (Charamel_lipgloss.Sides_color.all (Color.Indexed 238));
      title = Style.bold true (Style.foreground indigo no_style);
      description =
        Style.foreground (ld ~is_dark Color.Default (Color.Indexed 243)) no_style;
      error_indicator = Style.foreground red no_style;
      error_message = Style.foreground red no_style;
      select_selector = Style.foreground fuchsia no_style;
      next_indicator =
        Style.foreground fuchsia
          (Style.margin (Charamel_lipgloss.Sides.v ~left:1 ()) no_style);
      prev_indicator =
        Style.foreground fuchsia
          (Style.margin (Charamel_lipgloss.Sides.v ~right:1 ()) no_style);
      directory = Style.foreground indigo no_style;
      file = Style.foreground normal_fg no_style;
      card =
        focused0.base
        |> Style.border Charamel_lipgloss.Border.thick
        |> Style.border_top false |> Style.border_right false |> Style.border_bottom false
        |> Style.border_foreground (Charamel_lipgloss.Sides_color.all (Color.Indexed 238));
      next = Style.bold true (Style.foreground cream (Style.background fuchsia button));
      selected_option = Style.foreground green no_style;
      selected_prefix =
        Style.foreground (ld ~is_dark (hex "#02CF92") (hex "#02A877")) no_style;
      unselected_option = Style.foreground normal_fg no_style;
      unselected_prefix =
        Style.foreground (ld ~is_dark Color.Default (Color.Indexed 243)) no_style;
      focused_button =
        Style.bold true (Style.foreground cream (Style.background fuchsia button));
      blurred_button =
        Style.foreground normal_fg
          (Style.background (ld ~is_dark (Color.Indexed 252) (Color.Indexed 237)) button);
      note_title =
        Style.margin
          (Charamel_lipgloss.Sides.v ~bottom:1 ())
          (Style.bold true (Style.foreground indigo no_style));
      text_input =
        {
          cursor = Style.foreground green no_style;
          cursor_text = Style.foreground cream no_style;
          placeholder =
            Style.foreground
              (ld ~is_dark (Color.Indexed 248) (Color.Indexed 238))
              no_style;
          prompt = Style.foreground fuchsia no_style;
          text = Style.foreground normal_fg no_style;
        };
      indicators =
        {
          error_indicator = " *";
          select_selector = "> ";
          next_indicator = "->";
          prev_indicator = "<-";
          multi_select_selector = "> ";
          selected_prefix = "✓ ";
          unselected_prefix = "• ";
        };
    }
  in
  let blurred =
    let value = blurred_from_focused focused in
    {
      value with
      (* Charm keeps this selector visible in the unfocused multi-select view. *)
      multi_select_selector = Style.foreground fuchsia no_style;
      indicators = { value.indicators with multi_select_selector = "> " };
    }
  in
  let _, group_title, group_description =
    set_fields focused ~group_title:focused.title ~group_description:focused.description
  in
  {
    form_base = no_style;
    group_base = no_style;
    group_title;
    group_description;
    field_separator = "\n\n";
    focused;
    blurred;
    help = base_help ~is_dark:true;
  }

let dracula ~is_dark:_ =
  let background = hex "#282A36" in
  let selection = hex "#44475A" in
  let foreground = hex "#F8F8F2" in
  let comment = hex "#6272A4" in
  let green = hex "#50FA7B" in
  let purple = hex "#BD93F9" in
  let red = hex "#FF5555" in
  let yellow = hex "#F1FA8C" in
  let f = { (base_field ()) with base = focus_base; card = focus_base } in
  let focused =
    {
      f with
      base =
        f.base |> Style.border_foreground (Charamel_lipgloss.Sides_color.all selection);
      title = Style.foreground purple no_style;
      description = Style.foreground comment no_style;
      error_indicator = Style.foreground red no_style;
      error_message = Style.foreground red no_style;
      select_selector = Style.foreground yellow no_style;
      next_indicator =
        Style.foreground yellow
          (Style.margin (Charamel_lipgloss.Sides.v ~left:1 ()) no_style);
      prev_indicator =
        Style.foreground yellow
          (Style.margin (Charamel_lipgloss.Sides.v ~right:1 ()) no_style);
      directory = Style.foreground purple no_style;
      file = Style.foreground foreground no_style;
      card =
        f.base |> Style.border_foreground (Charamel_lipgloss.Sides_color.all selection);
      next = Style.foreground yellow no_style;
      selected_option = Style.foreground green no_style;
      selected_prefix = Style.foreground green no_style;
      unselected_option = Style.foreground foreground no_style;
      unselected_prefix = Style.foreground comment no_style;
      focused_button =
        Style.bold true (Style.foreground yellow (Style.background purple button));
      blurred_button = Style.foreground foreground (Style.background background button);
      note_title = Style.foreground purple no_style;
      text_input =
        {
          cursor = Style.foreground yellow no_style;
          cursor_text = Style.foreground foreground no_style;
          placeholder = Style.foreground comment no_style;
          prompt = Style.foreground yellow no_style;
          text = Style.foreground foreground no_style;
        };
    }
  in
  {
    form_base = Style.background background no_style;
    group_base = no_style;
    group_title = focused.title;
    group_description = focused.description;
    field_separator = "\n\n";
    focused;
    blurred = blurred_from_focused focused;
    help = base_help ~is_dark:true;
  }

let base16 ~is_dark:_ =
  let c n = Color.Indexed n in
  let f = { (base_field ()) with base = focus_base; card = focus_base } in
  let focused =
    {
      f with
      base = f.base |> Style.border_foreground (Charamel_lipgloss.Sides_color.all (c 8));
      title = Style.foreground (c 6) no_style;
      description = Style.foreground (c 8) no_style;
      error_indicator = Style.foreground (c 9) no_style;
      error_message = Style.foreground (c 9) no_style;
      select_selector = Style.foreground (c 3) no_style;
      next_indicator =
        Style.foreground (c 3)
          (Style.margin (Charamel_lipgloss.Sides.v ~left:1 ()) no_style);
      prev_indicator =
        Style.foreground (c 3)
          (Style.margin (Charamel_lipgloss.Sides.v ~right:1 ()) no_style);
      card = f.base |> Style.border_foreground (Charamel_lipgloss.Sides_color.all (c 8));
      next = Style.foreground (c 3) no_style;
      directory = Style.foreground (c 6) no_style;
      file = Style.foreground (c 7) no_style;
      multi_select_selector = Style.foreground (c 3) no_style;
      option_ = Style.foreground (c 7) no_style;
      selected_option = Style.foreground (c 2) no_style;
      selected_prefix = Style.foreground (c 2) no_style;
      unselected_option = Style.foreground (c 7) no_style;
      unselected_prefix = Style.foreground (c 7) no_style;
      focused_button = Style.foreground (c 7) (Style.background (c 5) button);
      blurred_button = Style.foreground (c 7) (Style.background (c 0) button);
      note_title = Style.foreground (c 6) no_style;
      text_input =
        {
          cursor = Style.foreground (c 5) no_style;
          cursor_text = Style.foreground (c 7) no_style;
          placeholder = Style.foreground (c 8) no_style;
          prompt = Style.foreground (c 3) no_style;
          text = Style.foreground (c 7) no_style;
        };
    }
  in
  let blurred =
    let b = blurred_from_focused focused in
    {
      b with
      title = Style.foreground (c 8) no_style;
      note_title = Style.foreground (c 8) no_style;
      text_input =
        {
          b.text_input with
          prompt = Style.foreground (c 8) no_style;
          text = Style.foreground (c 7) no_style;
        };
    }
  in
  {
    form_base = no_style;
    group_base = no_style;
    group_title = focused.title;
    group_description = focused.description;
    field_separator = "\n\n";
    focused;
    blurred;
    help = base_help ~is_dark:true;
  }

let catppuccin ~is_dark =
  let base_c = ld ~is_dark (hex "#EFF1F5") (hex "#1E1E2E") in
  let text_c = ld ~is_dark (hex "#4C4F69") (hex "#CDD6F4") in
  let subtext1 = ld ~is_dark (hex "#5C5F77") (hex "#BAC2DE") in
  let subtext0 = ld ~is_dark (hex "#6C6F85") (hex "#A6ADC8") in
  let overlay1 = ld ~is_dark (hex "#8C8FA1") (hex "#7F849C") in
  let overlay0 = ld ~is_dark (hex "#9CA0B0") (hex "#6C7086") in
  let green = ld ~is_dark (hex "#40A02B") (hex "#A6E3A1") in
  let red = ld ~is_dark (hex "#D20F39") (hex "#F38BA8") in
  let pink = ld ~is_dark (hex "#EA76CB") (hex "#F5C2E7") in
  let mauve = ld ~is_dark (hex "#8839EF") (hex "#CBA6F7") in
  let rosewater = ld ~is_dark (hex "#DC8A78") (hex "#F5E0DC") in
  let f = { (base_field ()) with base = focus_base; card = focus_base } in
  let focused =
    {
      f with
      base =
        f.base |> Style.border_foreground (Charamel_lipgloss.Sides_color.all subtext1);
      title = Style.foreground mauve no_style;
      description = Style.foreground subtext0 no_style;
      error_indicator = Style.foreground red no_style;
      error_message = Style.foreground red no_style;
      select_selector = Style.foreground pink no_style;
      next_indicator =
        Style.foreground pink
          (Style.margin (Charamel_lipgloss.Sides.v ~left:1 ()) no_style);
      prev_indicator =
        Style.foreground pink
          (Style.margin (Charamel_lipgloss.Sides.v ~right:1 ()) no_style);
      directory = Style.foreground mauve no_style;
      file = Style.foreground text_c no_style;
      card =
        f.base |> Style.border_foreground (Charamel_lipgloss.Sides_color.all subtext1);
      next = Style.foreground pink no_style;
      multi_select_selector = Style.foreground pink no_style;
      option_ = Style.foreground text_c no_style;
      selected_option = Style.foreground green no_style;
      selected_prefix = Style.foreground green no_style;
      unselected_option = Style.foreground text_c no_style;
      unselected_prefix = Style.foreground text_c no_style;
      focused_button = Style.foreground base_c (Style.background pink button);
      blurred_button = Style.foreground text_c (Style.background base_c button);
      note_title = Style.foreground mauve no_style;
      text_input =
        {
          cursor = Style.foreground rosewater no_style;
          cursor_text = Style.foreground text_c no_style;
          placeholder = Style.foreground overlay0 no_style;
          prompt = Style.foreground pink no_style;
          text = Style.foreground text_c no_style;
        };
    }
  in
  let blurred =
    let b = blurred_from_focused focused in
    {
      b with
      (* Catppuccin intentionally keeps navigation indicators visible while blurred. *)
      next_indicator = focused.next_indicator;
      prev_indicator = focused.prev_indicator;
      indicators =
        {
          b.indicators with
          next_indicator = focused.indicators.next_indicator;
          prev_indicator = focused.indicators.prev_indicator;
          multi_select_selector = "  ";
        };
    }
  in
  let help : Charamel_bubbles.Help.styles =
    {
      short_key = Style.foreground subtext0 no_style;
      short_desc = Style.foreground overlay1 no_style;
      short_separator = Style.foreground subtext0 no_style;
      full_key = Style.foreground subtext0 no_style;
      full_desc = Style.foreground overlay1 no_style;
      full_separator = Style.foreground subtext0 no_style;
      ellipsis = Style.foreground subtext0 no_style;
    }
  in
  {
    form_base = Style.background base_c no_style;
    group_base = no_style;
    group_title = focused.title;
    group_description = focused.description;
    field_separator = "\n\n";
    focused;
    blurred;
    help;
  }
