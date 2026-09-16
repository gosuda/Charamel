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

type action = Press | Release | Motion
type t = { x : int; y : int; button : button; action : action; mods : Key.mods }
