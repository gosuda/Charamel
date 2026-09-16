(** Interactive option selection for shell scripts.

    [choose] reads options from arguments or delimited standard input. Its terminal UI is
    rendered on the diagnostic stream while selected values are written to standard
    output. *)

type options = {
  options : string list;
  limit : int;
  no_limit : bool;
  ordered : bool;
  height : int;
  cursor : string;
  show_help : bool;
  timeout : float option;
  header : string;
  cursor_prefix : string;
  selected_prefix : string;
  unselected_prefix : string;
  selected : string list;
  select_if_one : bool;
  input_delimiter : string;
  output_delimiter : string;
  label_delimiter : string;
  strip_ansi : bool;
  padding : string;
  cursor_style : Gum_style.t;
  header_style : Gum_style.t;
  item_style : Gum_style.t;
  selected_style : Gum_style.t;
}
(** Configuration for [gum choose]. *)

val default_options : options
(** [default_options] is the default [gum choose] configuration. *)

type item
(** An option with its display label, output value, and selection order. *)

type msg
(** Messages consumed by the choose application. *)

type model
(** The choose application state. *)

val parse_options : delimiter:string -> string list -> (item list, string) result
(** [parse_options ~delimiter options] separates each option into a label and value at the
    first delimiter. With an empty delimiter, labels and values are identical. A missing
    delimiter is reported as an error. *)

val make : options -> model
(** [make options] creates an initial choose model after applying its limits and
    preselection. *)

val app : options -> (model, msg) Charm_tea.app
(** [app options] is the scripted or terminal choose application. *)

val selected : model -> string list
(** [selected model] returns selected output values in display order, or in selection
    order when [ordered] was requested. *)

val single_option : options -> (string option, string) result
(** [single_option options] returns [Some value] when [select_if_one] is enabled and the
    parsed option list contains exactly one item. It returns [None] when the shortcut does
    not apply. *)

val submitted : model -> bool
(** [submitted model] is [true] after Enter or Ctrl-Q submitted the model. *)

val run : Eio_unix.Stdenv.base -> options -> unit
(** [run env options] reads options, runs the terminal UI when required, and prints the
    selected values. *)

val cmd : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [cmd env] is the [gum choose] command. *)
