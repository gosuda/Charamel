(** Non-interactive Lip Gloss styling command. *)

val trim_lines : string -> string
(** [trim_lines text] trims ASCII and Unicode whitespace from each line while preserving
    the line structure. *)

val render : Gum_style.t -> trim:bool -> string -> string
(** [render style ~trim text] applies the validated gum style to [text]. *)

val cmd : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t
(** [cmd env] is the [style] subcommand evaluated with [env]. *)
