type mouse_mode = Mouse_off | Mouse_click | Mouse_motion | Mouse_all

type keyboard = {
  disambiguate : bool;
  report_events : bool;
  report_alternates : bool;
  report_all_keys : bool;
  report_text : bool;
}

type progress =
  | Progress_none
  | Progress_indeterminate
  | Progress_value of int
  | Progress_error of int
  | Progress_warning of int

type t = {
  content : string;
  cursor : Cursor.t option;
  alt_screen : bool;
  mouse : mouse_mode;
  bracketed_paste : bool;
  report_focus : bool;
  title : string option;
  keyboard : keyboard;
  background : Charm_ansi.Color.t option;
  foreground : Charm_ansi.Color.t option;
  progress : progress;
}

let default_keyboard =
  {
    disambiguate = true;
    report_events = false;
    report_alternates = false;
    report_all_keys = false;
    report_text = false;
  }

let v ?cursor ?(alt_screen = false) ?(mouse = Mouse_off) ?(bracketed_paste = true)
    ?(report_focus = false) ?title ?(keyboard = default_keyboard) ?background ?foreground
    ?(progress = Progress_none) content =
  {
    content;
    cursor;
    alt_screen;
    mouse;
    bracketed_paste;
    report_focus;
    title;
    keyboard;
    background;
    foreground;
    progress;
  }
