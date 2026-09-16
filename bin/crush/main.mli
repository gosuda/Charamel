(** Crush command-line entry point.

    The entry point exposes the command group and default terminal application to the
    executable wrapper. *)

val commands : (Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t) list
(** [commands] is the list of command constructors. *)

val default : Eio_unix.Stdenv.base -> unit Cmdliner.Term.t
(** [default env] is the terminal application term bound to [env]. *)

val run : unit -> unit
(** [run ()] initializes cryptographic randomness and runs Crush. *)
