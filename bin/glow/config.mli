(** Glow configuration.

    Configuration is loaded from JSON files, then environment variables and command line
    overrides are applied in that order. *)

type t = {
  style : string;
  width : int;
  pager : bool;
  tui : bool;
  all : bool;
  line_numbers : bool;
  preserve_new_lines : bool;
  mouse : bool;
}
(** The resolved Glow configuration. [width = 0] asks the caller to use the terminal
    width, capped at 120 columns. *)

type overrides = {
  style : string option;
  width : int option;
  pager : bool option;
  tui : bool option;
  all : bool option;
  line_numbers : bool option;
  preserve_new_lines : bool option;
  mouse : bool option;
}
(** Optional command-line values. A [None] value leaves the lower-precedence configuration
    unchanged. *)

type error =
  [ `Io of string * string | `Json of string * string | `Env of string * string ]
(** A configuration loading error. The first component identifies the source. *)

val default : t
(** [default] is the built-in configuration. *)

val empty_overrides : overrides
(** [empty_overrides] contains no command-line overrides. *)

val apply : t -> overrides -> t
(** [apply config overrides] applies the explicitly supplied overrides. *)

val config_path :
  explicit:string option -> cwd:string -> env:(string -> string option) -> string option
(** [config_path ~explicit ~cwd ~env] finds the explicit path, an upward Glow JSON file,
    or the XDG config file. It returns [None] when no file exists; the returned path is
    still useful to the config editor. *)

val load :
  explicit:string option ->
  cwd:string ->
  env:(string -> string option) ->
  read:(string -> (string, string) result option) ->
  (t * string option, error) result
(** [load ~explicit ~cwd ~env ~read] loads the first existing configuration. [read]
    returns [None] for a missing file and [Some (Error message)] for an I/O failure.
    Environment values override JSON values. *)

val default_json : string
(** [default_json] is the complete default configuration document. *)

val xdg_config_path : env:(string -> string option) -> string option
(** [xdg_config_path ~env] is the default XDG JSON path for Glow. *)

val parse_bool : string -> (bool, string) result
(** [parse_bool value] parses common shell boolean spellings. *)
