(** Structured and styled logging command. *)

type formatter = Text | Logfmt | Json  (** Output layouts accepted by [--formatter]. *)

type level =
  | None_
  | Debug
  | Info
  | Warn
  | Error
  | Fatal  (** Message levels accepted by [--level]. *)

val format_message : string -> string list -> string
(** [format_message format arguments] implements the printf mode's [%s], [%d], [%v], [%q],
    and [%%] verbs. Unknown verbs remain literal. *)

val time_formatter : string -> Ptime.t -> string
(** [time_formatter layout] converts Go-style layouts and named presets into a formatter
    suitable for {!Charamel_log.reporter}. *)

val emit :
  ?file:string ->
  ?formatter:formatter ->
  ?level:level ->
  ?min_level:string ->
  ?prefix:string ->
  ?time:string ->
  ?format:bool ->
  ?structured:bool ->
  ?styles:Charamel_log.Styles.t ->
  Charamel_cli.Env.t ->
  string list ->
  unit Lwt.t
(** [emit ... env text] emits one log record. This is the command's execution function;
    invalid levels and output failures report through [Charamel_cli]. *)

val cmd : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t
(** [cmd env] is the [log] subcommand evaluated with [env]. *)
