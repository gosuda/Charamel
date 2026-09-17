(** Mouse events.

    [t] is a single mouse report decoded from SGR (["CSI < Cb ; Cx ; Cy M/m"]) or X10
    (["CSI M Cb Cx Cy"]) mouse tracking sequences. Coordinates are 0-based, upper-left
    origin, matching {!Charamel_tea.Screen} and every other 0-based coordinate in this
    library; the wire protocols are 1-based and the decoder subtracts 1. *)

(** The button a click, release, or wheel report names. [None_] is X10's own release
    marker (SGR reports release with the button that was released instead); it never
    appears in a [Press] or [Motion] report. *)
type button =
  | Left
  | Middle
  | Right
  | Wheel_up
  | Wheel_down
  | Wheel_left
  | Wheel_right
  | Backward
  | Forward
  | Button_10
  | Button_11
  | None_

(** [Press] covers ordinary clicks and every wheel report (wheel buttons have no release
    event on the wire, so a wheel report is always [Press]). [Release] is a button-up
    report; [Motion] is drag or hover reporting, sent only when motion tracking is
    enabled. *)
type action = Press | Release | Motion

type t = {
  x : int;
  y : int;
  button : button;
  action : action;
  mods : Key.mods;
      (** Only [shift], [alt], and [ctrl] can be set: the mouse wire protocols carry no
          other modifier bits. *)
}
