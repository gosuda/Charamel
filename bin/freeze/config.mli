(** Configuration and command-line precedence for [freeze]. *)

type border = { radius : float; width : float; color : string }
(** A window border configuration. *)

type shadow = { blur : float; x : float; y : float }
(** A drop-shadow configuration. *)

type font = { family : string; file : string; size : float; ligatures : bool }
(** A font configuration. *)

type t = {
  input : string;
  background : string;
  margin : float list;
  padding : float list;
  window : bool;
  width : float;
  height : float;
  config : string;
  interactive : bool;
  language : string;
  theme : string;
  wrap : int;
  output : string;
  execute : string;
  execute_timeout : float;
  border : border;
  shadow : shadow;
  font : font;
  line_height : float;
  lines : int list;
  show_line_numbers : bool;
}
(** The effective configuration after JSON and command-line layers. *)

type cli = {
  input : string;
  background : string option;
  margin : float list option;
  padding : float list option;
  window : bool option;
  width : float option;
  height : float option;
  config : string;
  interactive : bool;
  language : string option;
  theme : string option;
  wrap : int option;
  output : string option;
  execute : string option;
  execute_timeout : float option;
  border_radius : float option;
  border_width : float option;
  border_color : string option;
  shadow_blur : float option;
  shadow_x : float option;
  shadow_y : float option;
  font_family : string option;
  font_file : string option;
  font_size : float option;
  font_ligatures : bool option;
  line_height : float option;
  lines : int list option;
  show_line_numbers : bool;
}
(** Values supplied by command-line arguments. [None] means absent, allowing a JSON value
    to remain effective. *)

val default : t
(** [default] is the base preset from freeze. *)

val cli_term : cli Cmdliner.Term.t
(** [cli_term] parses the positional input and all freeze options. *)

val load : fs_root:string -> name:string -> (t, string) result Lwt.t
(** [load ~fs_root ~name] loads a named preset, user JSON, or a path. Unknown preset names
    fall back to the base preset, as the upstream command does. *)

val apply_cli : t -> cli -> t
(** [apply_cli base cli] applies command-line values over [base]. *)

val expand_sides : scale:float -> float list -> float array
(** [expand_sides ~scale values] expands one, two, or four values to top/right/bottom/left
    order. Other lengths produce four zeroes. *)

val save_user : t -> (unit, string) result Lwt.t
(** [save_user config] writes the JSON-compatible settings to the freeze user
    configuration path. The path is always the fixed XDG location, so no filesystem root
    is needed to resolve it. *)
