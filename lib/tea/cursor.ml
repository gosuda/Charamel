type shape = Block | Underline | Bar

type t = {
  row : int;
  col : int;
  shape : shape;
  blink : bool;
  color : Charamel_ansi.Color.t option;
}

let[@warning "-16"] v ?(shape = Block) ?(blink = true) ?color row col =
  { row; col; shape; blink; color }
