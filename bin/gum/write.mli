(** Multi-line interactive text input.

    [write] uses the textarea component with Ctrl-J for a newline and Enter for
    submission. The UI is rendered on the diagnostic stream and the submitted text is
    written to standard output. *)

type cursor_mode = Blink | Hide | Static  (** The terminal cursor mode. *)

type options = {
  width : int;
  height : int;
  header : string;
  placeholder : string;
  prompt : string;
  show_cursor_line : bool;
  show_line_numbers : bool;
  value : string;
  char_limit : int;
  max_lines : int;
  show_help : bool;
  cursor_mode : cursor_mode;
  timeout : float option;
  strip_ansi : bool;
  padding : string;
  base_style : Gum_style.t;
  cursor_line_number_style : Gum_style.t;
  cursor_line_style : Gum_style.t;
  cursor_style : Gum_style.t;
  end_of_buffer_style : Gum_style.t;
  line_number_style : Gum_style.t;
  header_style : Gum_style.t;
  placeholder_style : Gum_style.t;
  prompt_style : Gum_style.t;
}
(** Configuration for [gum write]. *)

val default_options : options
(** [default_options] is the default [gum write] configuration. *)

type msg
(** Messages consumed by the write application. *)

type model
(** The write application state. *)

val initial_value : Eio_unix.Stdenv.base -> options -> string
(** [initial_value env options] uses [options.value], or a non-empty standard input value
    with carriage returns removed when [value] is empty. *)

val normalize_lines : max_lines:int -> string -> string
(** [normalize_lines ~max_lines text] keeps at most [max_lines] logical lines; a
    non-positive limit leaves [text] unchanged. *)

val make : options -> model
(** [make options] creates a focused textarea model. *)

val app : options -> (model, msg) Charamel_tea.app
(** [app options] is the scripted or terminal write application. *)

val value : model -> string
(** [value model] is the current textarea content. *)

val submitted : model -> bool
(** [submitted model] is [true] after Enter submitted the content. *)

val run : Eio_unix.Stdenv.base -> options -> unit
(** [run env options] runs the write UI and writes the submitted content. *)

val cmd : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [cmd env] is the [gum write] command. *)
