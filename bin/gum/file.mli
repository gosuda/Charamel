(** Interactive file and directory picker. *)

type options = {
  path : string;
  cursor : string;
  all : bool;
  permissions : bool;
  size : bool;
  file : bool;
  directory : bool;
  show_help : bool;
  timeout : float option;
  header : string;
  height : int;
  padding : string;
  cursor_style : Gum_style.t;
  symlink_style : Gum_style.t;
  directory_style : Gum_style.t;
  file_style : Gum_style.t;
  permissions_style : Gum_style.t;
  selected_style : Gum_style.t;
  file_size_style : Gum_style.t;
  header_style : Gum_style.t;
}
(** Configuration for [gum file]. *)

val default_options : options
(** [default_options] is the command's default configuration. *)

val run : Eio_unix.Stdenv.base -> options -> unit
(** [run env options] runs the picker and prints the selected absolute path. *)

val cmd : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [cmd env] is the [gum file] command. *)
