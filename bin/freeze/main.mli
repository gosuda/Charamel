(** Freeze command entry point. *)

val run : Charamel_cli.Env.t -> Freeze_core.Config.cli -> unit Lwt.t
(** [run env cli] runs one freeze invocation under the supplied environment. *)
