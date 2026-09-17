type t = {
  top : Charamel_ansi.Color.t option;
  right : Charamel_ansi.Color.t option;
  bottom : Charamel_ansi.Color.t option;
  left : Charamel_ansi.Color.t option;
}

let none = { top = None; right = None; bottom = None; left = None }

let all color =
  { top = Some color; right = Some color; bottom = Some color; left = Some color }

let v ?top ?right ?bottom ?left () = { top; right; bottom; left }
