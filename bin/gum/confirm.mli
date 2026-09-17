(** Interactive affirmative or negative confirmation.

    [confirm] renders its prompt on the diagnostic stream. A piped first line is accepted
    without opening a terminal. Affirmative answers exit zero and negative answers exit
    one. *)

type options = {
  default : bool;
  show_output : bool;
  affirmative : string;
  negative : string;
  prompt : string;
  show_help : bool;
  timeout : float option;
  padding : string;
  prompt_style : Gum_style.t;
  selected_style : Gum_style.t;
  unselected_style : Gum_style.t;
}
(** Configuration for [gum confirm]. *)

val default_options : options
(** [default_options] is the default [gum confirm] configuration. *)

type msg
(** Messages consumed by the confirmation application. *)

type model
(** The confirmation application state. *)

val make : options -> model
(** [make options] creates a confirmation model with its default answer. *)

val app : options -> (model, msg) Charamel_tea.app
(** [app options] is the scripted or terminal confirmation application. *)

val answer : model -> bool
(** [answer model] is the answer currently selected by the user. *)

val submitted : model -> bool
(** [submitted model] is [true] after an answer was submitted or cancelled. *)

val run : Eio_unix.Stdenv.base -> options -> unit
(** [run env options] accepts a piped answer or runs the terminal UI. *)

val cmd : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [cmd env] is the [gum confirm] command. *)
