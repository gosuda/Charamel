(** Countdown timer driven by a declarative periodic subscription. *)

(** Divergence from upstream bubbles [timer]: upstream also defines a [Timeout] message
    that its [Update] emits once when the countdown reaches zero. This port has no such
    variant: [Tick] only decrements, and expiry is observed by calling [timed_out] on the
    model returned from the [Tick] update. Applications that need a one-shot message on
    expiry dispatch it themselves after seeing [timed_out] flip from [false] to [true]. *)
type msg = Tick | Start | Stop | Toggle

type t

val v : ?interval:float -> timeout:float -> unit -> t
val update : msg -> t -> t * msg Charamel_tea.Cmd.t
val view : t -> string
val key : t -> Charamel_tea.Key.t -> msg option
val subscriptions : t -> msg Charamel_tea.Sub.t
val running : t -> bool
val timed_out : t -> bool
val timeout : t -> float
val start : t -> t
val stop : t -> t
val toggle : t -> t
