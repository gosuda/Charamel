(** Animated terminal spinner frames. *)

type kind =
  | Line
  | Dot
  | Mini_dot
  | Jump
  | Pulse
  | Points
  | Globe
  | Moon
  | Monkey
  | Meter
  | Hamburger
  | Ellipsis

val frames : kind -> string list
val fps : kind -> float
val kind_of_string : string -> kind option

type msg = Tick
type t

val v :
  ?kind:kind ->
  ?frames:string list * float ->
  ?style:Charamel_lipgloss.Style.t ->
  unit ->
  t

val update : msg -> t -> t * msg Charamel_tea.Cmd.t
val view : t -> string
val key : t -> Charamel_tea.Key.t -> msg option
val subscriptions : t -> msg Charamel_tea.Sub.t
val set_kind : kind -> t -> t
val set_style : Charamel_lipgloss.Style.t -> t -> t
val kind : t -> kind option
