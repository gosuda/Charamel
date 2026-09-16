(** Non-interactive text formatting command.

    [gum format] accepts text arguments or standard input and renders markdown, code,
    emoji, or the small styling template language. *)

type kind = Markdown | Template | Code | Emoji  (** Supported formatting modes. *)

type error = [ `Msg of string ]
(** Rendering errors. *)

val render :
  ?theme:string ->
  ?language:string ->
  ?strip_ansi:bool ->
  kind ->
  string ->
  (string, error) result
(** [render ?theme ?language ?strip_ansi kind text] renders [text]. [theme] defaults to
    [pink], [language] to the empty language, and [strip_ansi] only affects callers that
    choose to strip input before calling this pure function. *)

val cmd : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [cmd env] is the [format] subcommand evaluated with [env]. *)
