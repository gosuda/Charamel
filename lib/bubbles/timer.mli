(** Countdown timer driven by a declarative periodic subscription. *)

type msg = Tick | Start | Stop | Toggle
type t

val v : ?interval:float -> timeout:float -> unit -> t
val update : msg -> t -> t * msg Charm_tea.Cmd.t
val view : t -> string
val key : t -> Charm_tea.Key.t -> msg option
val subscriptions : t -> msg Charm_tea.Sub.t
val running : t -> bool
val timed_out : t -> bool
val timeout : t -> float
val start : t -> t
val stop : t -> t
val toggle : t -> t
