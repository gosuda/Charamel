(** Fuzzy and exact option filtering for shell scripts.

    [filter] reads candidates from arguments, standard input, or the current directory.
    The interactive view is rendered on the diagnostic stream and selected values are
    written to standard output. *)

type options = {
  options : string list;
  indicator : string;
  limit : int;
  no_limit : bool;
  select_if_one : bool;
  selected : string list;
  show_help : bool;
  strict : bool;
  selected_prefix : string;
  unselected_prefix : string;
  header : string;
  placeholder : string;
  prompt : string;
  width : int;
  height : int;
  value : string;
  reverse : bool;
  fuzzy : bool;
  fuzzy_sort : bool;
  timeout : float option;
  input_delimiter : string;
  output_delimiter : string;
  strip_ansi : bool;
  padding : string;
  indicator_style : Gum_style.t;
  selected_prefix_style : Gum_style.t;
  unselected_prefix_style : Gum_style.t;
  header_style : Gum_style.t;
  text_style : Gum_style.t;
  cursor_text_style : Gum_style.t;
  match_style : Gum_style.t;
  prompt_style : Gum_style.t;
  placeholder_style : Gum_style.t;
}
(** Configuration for [gum filter]. *)

val default_options : options
(** [default_options] is the default [gum filter] configuration. *)

type match_ = { text : string; value : string; matched : int list; score : int }
(** A candidate and the grapheme indices matched in its display text. *)

type msg
(** Messages consumed by the filter application. *)

type model
(** The filter application state. *)

val matched_ranges : int list -> (int * int) list
(** [matched_ranges indices] coalesces adjacent grapheme indices into inclusive ranges. *)

val exact_matches : pattern:string -> string list -> match_ list
(** [exact_matches ~pattern candidates] performs case-insensitive substring matching in
    input order. *)

val make : options -> model
(** [make options] creates a filter model with its initial query and matches. *)

val app : options -> (model, msg) Charamel_tea.app
(** [app options] is the scripted or terminal filter application. *)

val single_option : options -> (string option, string) result
(** [single_option options] returns [Some value] when [select_if_one] is enabled, an
    initial query is present, and exactly one candidate matches. [None] when the shortcut
    does not apply. *)

val selected : model -> string list
(** [selected model] returns selected output values in input order. *)

val submitted : model -> bool
(** [submitted model] is [true] after Enter or Ctrl-Q submitted the model. *)

val run : Eio_unix.Stdenv.base -> options -> unit
(** [run env options] reads candidates, runs filtering, and prints the result. *)

val cmd : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [cmd env] is the [gum filter] command. *)
