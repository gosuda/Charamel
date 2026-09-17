(** Per-edge colors for borders.

    Each optional edge is independent; [None] leaves that edge at its inherited color. *)

type t = {
  top : Charamel_ansi.Color.t option;
  right : Charamel_ansi.Color.t option;
  bottom : Charamel_ansi.Color.t option;
  left : Charamel_ansi.Color.t option;
}

val none : t
(** [none] leaves every edge uncolored. *)

val all : Charamel_ansi.Color.t -> t
(** [all color] assigns [color] to every edge. *)

val v :
  ?top:Charamel_ansi.Color.t ->
  ?right:Charamel_ansi.Color.t ->
  ?bottom:Charamel_ansi.Color.t ->
  ?left:Charamel_ansi.Color.t ->
  unit ->
  t
(** [v ?top ?right ?bottom ?left ()] assigns the supplied edge colors. *)
