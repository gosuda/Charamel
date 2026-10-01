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
  Charamel_cli.Env.t ->
  command:string list ->
  timeout:float option ->
  (child_result, string) result Lwt.t
(** [run_child env ~command ~timeout] runs [command] with inherited stdin, drains stdout
    and stderr concurrently, preserving each stream byte-for-byte. [result.output] is the
    combined view in observed read-arrival order; no ordering is promised between the
    separate operating-system streams. *)

val run_pty_pair :
  ?rows:int ->
  ?cols:int ->
  ?stdin_text:string ->
  command:string list ->
  timeout:float option ->
  unit ->
  (child_result, string) result Lwt.t
(** [run_pty_pair ?rows ?cols ?stdin_text ~command ~timeout ()] runs [command] on the
    two-PTY capture path {!val:run_child} takes when standard output is a terminal: the
    child's stdout and stderr are separate pseudo-terminals sized [rows] by [cols]
    (defaulting to the real terminal, else 24x80), and [stdin_text], when given, is
    written to the child's stdin pipe and closed, so a real [read] sees it. Exposed
    because [dune runtest] connects a pipe, which would otherwise leave that path
    unexercised. *)

val run : Charamel_cli.Env.t -> options -> unit Lwt.t
(** [run env options] runs the configured child and routes its output. *)

val cmd : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t
(** [cmd env] is the [gum spin] command. *)
