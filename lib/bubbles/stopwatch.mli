(** Stopwatch state driven by a declarative periodic subscription. *)

type msg = Tick | Start | Stop | Reset
type t

val v : ?interval:float -> unit -> t
val update : msg -> t -> t * msg Charm_tea.Cmd.t
val view : t -> string
val key : t -> Charm_tea.Key.t -> msg option
val subscriptions : t -> msg Charm_tea.Sub.t
val elapsed : t -> float
val running : t -> bool
val start : t -> t
val stop : t -> t
val toggle : t -> t
val reset : t -> t
