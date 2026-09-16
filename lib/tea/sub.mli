(** Subscriptions: declarative interest in events external to [update].

    Building a subscription starts nothing. [t] is a pure description; the program's
    runtime diffs the subscription set the model's [subscriptions] function returns after
    every update and starts or stops the fibers that back it. [Charm_tea] re-exports [t]
    abstract. Constructors are [private]: every value of [t] is built through the
    functions below, and any module may still pattern-match one. *)

type 'msg t = private
  | None_
  | Batch of 'msg t list
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Key of (Key.t -> 'msg)
  | Key_release of (Key.t -> 'msg)
  | Mouse of (Mouse.t -> 'msg)
  | Paste of (string -> 'msg)
  | Focus of ([ `Focused | `Blurred ] -> 'msg)
  | Resize of (rows:int -> cols:int -> 'msg)
  | Every of float * (Mtime.t -> 'msg)
  | Terminal of (Event.t -> 'msg)
      (** The type for a subscription.

          [Every] is keyed by its interval: the runtime runs one timer fiber per distinct
          interval across the whole subscription tree, started and stopped as the interval
          set changes from one update to the next. [Map] transforms every message a
          subscription produces. *)

val none : 'msg t
(** [none] expresses no interest in external events. *)

val batch : 'msg t list -> 'msg t
(** [batch subs] subscribes to every event source in [subs]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f sub] is [sub] with every message it produces passed through [f]. *)

val key : (Key.t -> 'msg) -> 'msg t
(** [key handler] delivers key presses and key repeats to [handler]. *)

val key_release : (Key.t -> 'msg) -> 'msg t
(** [key_release handler] delivers key releases to [handler]. *)

val mouse : (Mouse.t -> 'msg) -> 'msg t
(** [mouse handler] delivers mouse events to [handler]. *)

val paste : (string -> 'msg) -> 'msg t
(** [paste handler] delivers bracketed-paste payloads to [handler]. *)

val focus : ([ `Focused | `Blurred ] -> 'msg) -> 'msg t
(** [focus handler] delivers terminal focus and blur reports to [handler]. *)

val resize : (rows:int -> cols:int -> 'msg) -> 'msg t
(** [resize handler] delivers the new terminal size to [handler]. *)

val every : float -> (Mtime.t -> 'msg) -> 'msg t
(** [every seconds handler] delivers the tick time to [handler] once every [seconds]. *)

val terminal : (Event.t -> 'msg) -> 'msg t
(** [terminal handler] delivers terminal reports to [handler]: replies to {!val:Cmd.query}
    and any input the runtime could not classify. *)
