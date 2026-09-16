(** Small, deterministic templates used by [gum format --type template].

    A template consists of literal text and [{{ Func args }}] actions. The supported
    actions are the colour and text-attribute functions documented by gum; arguments are
    quoted strings or nested actions. *)

type error = [ `Msg of string ]
(** Template parse or evaluation errors. *)

val render : string -> (string, error) result
(** [render source] evaluates [source]. [{{-] and [-}}] trim surrounding whitespace in the
    same way as the Go template syntax. *)
