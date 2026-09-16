(** Per-edge colors for borders.

    Each optional edge is independent; [None] leaves that edge at its inherited color. *)

type t = {
  top : Charm_ansi.Color.t option;
  right : Charm_ansi.Color.t option;
  bottom : Charm_ansi.Color.t option;
  left : Charm_ansi.Color.t option;
}

val none : t
(** [none] leaves every edge uncolored. *)

val all : Charm_ansi.Color.t -> t
(** [all color] assigns [color] to every edge. *)

val v :
  ?top:Charm_ansi.Color.t ->
  ?right:Charm_ansi.Color.t ->
  ?bottom:Charm_ansi.Color.t ->
  ?left:Charm_ansi.Color.t ->
  unit ->
  t
(** [v ?top ?right ?bottom ?left ()] assigns the supplied edge colors. *)
