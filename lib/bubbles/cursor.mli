(** A blinking cursor for line-editing components.

    [t] is immutable from a caller's perspective: every operation returns the next cursor
    value. The cursor can be rendered virtually inside component text or exposed as a real
    terminal cursor by its parent. *)

type mode = Blink | Static | Hide  (** The cursor display mode. *)
type msg = Tick  (** The periodic message consumed by {!update}. *)

type t
(** The cursor state. *)

val v :
  ?blink_speed:float ->
  ?style:Charamel_lipgloss.Style.t ->
  ?text_style:Charamel_lipgloss.Style.t ->
  unit ->
  t
(** [v ?blink_speed ?style ?text_style ()] constructs an unfocused blinking cursor. The
    default blink period is [0.53] seconds; styles default to empty styles. *)

val update : msg -> t -> t
(** [update Tick t] advances the blink state. The first tick after [show] is absorbed so
    movement does not immediately hide the cursor. *)

val subscriptions : t -> msg Charamel_tea.Sub.t
(** [subscriptions t] subscribes to ticks only when [t] is focused and blinking. *)

val view : t -> string
(** [view t] renders the character under the cursor using the current blink state. *)

val focus : t -> t
(** [focus t] focuses [t] and makes it visible unless its mode is [Hide]. *)

val blur : t -> t
(** [blur t] removes focus and hides the cursor. *)

val focused : t -> bool
(** [focused t] reports whether [t] is focused. *)

val mode : t -> mode
(** [mode t] is the current display mode. *)

val set_mode : mode -> t -> t
(** [set_mode mode t] changes the display mode and resets visibility accordingly. *)

val set_char : string -> t -> t
(** [set_char char t] sets the text cell displayed by [t]. *)

val set_style : Charamel_lipgloss.Style.t -> t -> t
(** [set_style style t] sets the visible cursor style. *)

val set_text_style : Charamel_lipgloss.Style.t -> t -> t
(** [set_text_style style t] sets the style used while the cursor is hidden. *)

val blink_speed : t -> float
(** [blink_speed t] is the tick interval in seconds. *)

val set_blink_speed : float -> t -> t
(** [set_blink_speed seconds t] sets the tick interval. Non-positive values are clamped to
    a small positive interval. *)

val show : t -> t
(** [show t] shows the cursor immediately and absorbs the next tick. *)

val is_blinked : t -> bool
(** [is_blinked t] reports whether the cursor is in its hidden blink phase. *)
