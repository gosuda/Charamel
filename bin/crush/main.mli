(** Crush command-line entry point.

    The entry point exposes the command group and default terminal application to the
    executable wrapper. *)

val commands : (Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t) list
(** [commands] is the list of command constructors. *)

val default : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Term.t
(** [default env] is the terminal application term bound to [env]. *)

val run : unit -> unit
(** [run ()] initializes cryptographic randomness and runs Crush. *)
