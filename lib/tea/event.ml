type t =
  | Key of Key.t
  | Mouse of Mouse.t
  | Paste of string
  | Focus
  | Blur
  | Resize of { rows : int; cols : int }
  | Cursor_position of { row : int; col : int }
  | Background_color of Charm_ansi.Color.t
  | Foreground_color of Charm_ansi.Color.t
  | Cursor_color of Charm_ansi.Color.t
  | Terminal_version of string
  | Kitty_flags of int
  | Mode_report of { mode : int; value : int }
  | Profile of Charm_colorprofile.t
  | Unknown of string
