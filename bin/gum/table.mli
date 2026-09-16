(** CSV table command. *)

type error =
  [ `Csv of Csv.error
  | `No_data
  | `Invalid_width of int
  | `Invalid_columns
  | `Invalid_padding of string
  | `Invalid_return_column of int ]

type options = {
  separator : string;
  columns : string list;
  widths : int list;
  height : int;
  print : bool;
  file : string;
  border : string;
  show_help : bool;
  hide_count : bool;
  lazy_quotes : bool;
  fields_per_record : int;
  return_column : int;
  timeout : float option;
  padding : string;
  border_style : Gum_style.t;
  cell_style : Gum_style.t;
  header_style : Gum_style.t;
  selected_style : Gum_style.t;
}
(** Configuration for [gum table]. *)

val default_options : options
(** [default_options] is the command's default configuration. *)

val parse_input : options -> string -> (string list * string list list, error) result
(** [parse_input options input] parses [input] into headers and padded rows. *)

val render_static :
  options -> headers:string list -> rows:string list list -> (string, error) result
(** [render_static options ~headers ~rows] renders a non-interactive table, or reports
    malformed padding at the raw option boundary. *)

val error_message : error -> string
(** [error_message error] formats a table error. *)

val run : Eio_unix.Stdenv.base -> options -> unit
(** [run env options] reads, selects, and writes one table row. *)

val cmd : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [cmd env] is the [gum table] command. *)
