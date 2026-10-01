(** Gum command registry and executable entry point. *)

val commands : (Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t) list
(** [commands] are the fourteen visible gum subcommands; {!run} supplies each maker with
    the runtime environment. *)

val run : unit -> unit
(** [run ()] starts the gum command-line runtime with global verbosity and Cmdliner
    [--version] handling supplied by {!Charamel_cli.run}. *)
