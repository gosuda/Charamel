(** Gum command registry and executable entry point. *)

val commands : (Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t) list
(** [commands] are the fourteen visible gum subcommands; {!run} supplies each maker with
    the runtime environment. *)

val run : unit -> unit
(** [run ()] starts the gum command-line runtime with global verbosity and Cmdliner
    [--version] handling supplied by {!Charm_cli.run}. *)
