(** Shared application runtime and paths. *)

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
  ?default:(Eio_unix.Stdenv.base -> unit Cmdliner.Term.t) ->
  (Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t) list ->
  unit
(** [run ~name ~version ~doc ?default commands] runs an application.

    [commands] are grouped Cmdliner subcommands. [default], when present, supplies the
    term for a single invocation with no subcommand. The standard [--help] and [--version]
    exits are [0]. Command diagnostics use [1], usage errors use [2], timeouts use [124],
    and interrupts use [130]. [ -v ] and [ -q ] are accepted as global verbosity controls
    and are removed before Cmdliner evaluates the command. The runtime owns the one
    [Eio_main.run] invocation, installs the colour-aware [Charm_log] reporter for the
    command lifetime, and exits only after Eio resources have unwound. *)
