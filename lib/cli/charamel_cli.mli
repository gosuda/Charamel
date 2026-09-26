(** Shared application runtime and paths. *)

module Env = Env
(** [Env.t] is the working directory, filesystem root, channels and clock a command is
    handed. *)

module Xdg = Xdg
(** [Xdg] computes per-application XDG base directories. *)

module Version = Version
(** [Version.current] is the build version, or ["dev"]. *)

val is_dark : env:(string -> string option) -> bool
(** [is_dark ~env] selects the terminal background polarity from the [COLORFGBG]
    environment hint. The final semicolon-separated field is interpreted as an xterm
    256-colour background index and classified using the standard palette's relative
    luminance. Missing, malformed, or out-of-range hints select dark. This function never
    queries the terminal, so it is safe for startup code and remote sessions. *)

val nearest_candidate : candidates:string list -> string -> string option
(** [nearest_candidate ~candidates query] returns the closest candidate under Levenshtein
    distance, when that distance is at most [3]. Ties keep the input order. The empty
    candidate list and queries with no candidate within the bound return [None]. *)

val error : ?code:int -> string -> 'a
(** [error ?code message] aborts the current command with a diagnostic. [code] defaults to
    [1] and must be in [0..255]. {!run} prints [message] and maps the command to [code]
    after command-owned resources unwind. *)

val exit : int -> 'a
(** [exit code] aborts the current command without a diagnostic. [code] must be in
    [0..255]. {!run} maps it to [code] after command-owned resources unwind. *)

val run :
  name:string ->
  version:string ->
  doc:string ->
  ?default:(Env.t -> unit Lwt.t Cmdliner.Term.t) ->
  (Env.t -> unit Lwt.t Cmdliner.Cmd.t) list ->
  unit
(** [run ~name ~version ~doc ?default commands] runs an application.

    [commands] are grouped Cmdliner subcommands. [default], when present, supplies the
    term for a single invocation with no subcommand. Every maker is handed the same
    {!Env.t}, built once from the process's own directory, channels and clock. The
    standard [--help] and [--version] exits are [0]. Command diagnostics use [1], usage
    errors use [2], timeouts use [124], and interrupts use [130]. [ -v ] and [ -q ] are
    accepted as global verbosity controls and are removed before Cmdliner evaluates the
    command. The runtime owns the one [Lwt_main.run] invocation, installs the colour-aware
    [Charamel_log] reporter for the command lifetime, and exits only after the command's
    promise has resolved and its output has been flushed. *)
