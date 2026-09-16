(** Freeze command entry point. *)

val run : Eio_unix.Stdenv.base -> Freeze_core.Config.cli -> unit
(** [run env cli] runs one freeze invocation under the supplied Eio environment. *)
