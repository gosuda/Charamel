(** Single-line interactive text input.

    The UI is rendered on the diagnostic stream and the submitted value is written
    unmodified to standard output. Piped input is used as the initial value while keyboard
    input still comes from the terminal. *)

type cursor_mode = Blink | Hide | Static  (** The terminal cursor mode. *)

type options = {
  placeholder : string;
  prompt : string;
  cursor_mode : cursor_mode;
  value : string;
  char_limit : int;
  width : int;
  password : bool;
  show_help : bool;
  header : string;
  timeout : float option;
  strip_ansi : bool;
  padding : string;
  prompt_style : Gum_style.t;
  placeholder_style : Gum_style.t;
  cursor_style : Gum_style.t;
  header_style : Gum_style.t;
}
(** Configuration for [gum input]. *)

val default_options : options
(** [default_options] is the default [gum input] configuration. *)

type msg
(** Messages consumed by the input application. *)

type model
(** The input application state. *)

val initial_value : Charamel_cli.Env.t -> options -> string Lwt.t
(** [initial_value env options] uses [options.value], or a non-empty trimmed
    standard-input value when the option is empty. *)

val make : options -> model
(** [make options] creates a focused input model. *)

val app : options -> (model, msg) Charamel_tea.app
(** [app options] is the scripted or terminal input application. *)

val value : model -> string
(** [value model] is the current input text. *)

val submitted : model -> bool
(** [submitted model] is [true] after Enter submitted the value. *)

val run : Charamel_cli.Env.t -> options -> unit Lwt.t
(** [run env options] runs the input UI and writes the submitted value. *)

val cmd : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t
(** [cmd env] is the [gum input] command. *)
