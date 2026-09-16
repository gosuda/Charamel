(** Non-interactive layout joining command. *)

val join :
  ?align:string ->
  ?horizontal:bool ->
  ?vertical:bool ->
  string list ->
  (string, [ `Msg of string ]) result
(** [join ?align ?horizontal ?vertical texts] joins at least one text. The default
    direction is horizontal; [vertical] wins when both directions are requested. [align]
    defaults to [left] and accepts left, center, right, top, bottom, or middle. *)

val cmd : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [cmd env] is the [join] subcommand evaluated with [env]. *)
