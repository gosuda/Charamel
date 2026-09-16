(** Run a child process while displaying a spinner. *)

type options = {
  command : string list;
  show_output : bool;
  show_error : bool;
  show_stdout : bool;
  show_stderr : bool;
  spinner : string;
  title : string;
  align : string;
  timeout : float option;
  padding : string;
  spinner_style : Gum_style.t;
  title_style : Gum_style.t;
}
(** Configuration for [gum spin]. *)

type child_result = {
  status : int;
  stdout : string;
  stderr : string;
  output : string;
  timed_out : bool;
}
(** Captured child output and normalized exit status. *)

val default_options : options
(** [default_options] is the command's default configuration. *)

val run_child :
  ?capture:bool ->
  Eio_unix.Stdenv.base ->
  command:string list ->
  timeout:float option ->
  (child_result, string) result
(** [run_child env ~command ~timeout] runs [command] with inherited stdin, drains stdout
    and stderr concurrently, preserving each stream byte-for-byte. [result.output] is the
    combined view in observed read-arrival order; no ordering is promised between the
    separate operating-system streams. *)

val run : Eio_unix.Stdenv.base -> options -> unit
(** [run env options] runs the configured child and routes its output. *)

val cmd : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [cmd env] is the [gum spin] command. *)
