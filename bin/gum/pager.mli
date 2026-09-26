(** Interactive terminal pager with search and soft wrapping.

    [pager] renders its complete view on the diagnostic stream. It consumes content from
    an optional argument or standard input and writes no result to standard output. *)

type options = {
  content : string;
  style : Gum_style.t;
  show_line_numbers : bool;
  soft_wrap : bool;
  timeout : float option;
  line_number_style : Gum_style.t;
  match_style : Gum_style.t;
  match_highlight_style : Gum_style.t;
}
(** Configuration for [gum pager]. *)

val default_options : options
(** [default_options] is the default [gum pager] configuration. *)

type msg
(** Messages consumed by the pager application. *)

type model
(** The pager application state. *)

val sanitize : string -> string
(** [sanitize text] removes backspace-overwrite pairs from [text]. *)

val search_lines : pattern:string -> string -> int list
(** [search_lines ~pattern content] returns zero-based line numbers whose text contains
    [pattern], case-insensitively. An invalid or empty pattern returns the empty list. *)

val make : options -> model
(** [make options] creates a pager model. *)

val app : options -> (model, msg) Charamel_tea.app
(** [app options] is the scripted or terminal pager application. *)

val run : Charamel_cli.Env.t -> options -> unit Lwt.t
(** [run env options] obtains content and runs the pager until quit. *)

val cmd : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t
(** [cmd env] is the [gum pager] command. *)
