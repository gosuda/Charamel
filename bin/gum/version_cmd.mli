(* Gum version display and semantic-version check command. *)

val check : current:string -> string -> (unit, [ `Msg of string ]) result
(** [check ~current constraint_text] succeeds when [current] satisfies the supplied
    Semantic Versioning constraint. The development version ["dev"] cannot be checked. *)

val display : current:string -> string option -> (string, [ `Msg of string ]) result
(** [display ~current constraint] returns [current] when [constraint] is absent, and the
    empty string after a successful check when it is present. *)

val cmd : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [cmd env] is the [version] subcommand. With no positional constraint it prints the
    current build version; with one it performs a check and prints nothing on success. *)
