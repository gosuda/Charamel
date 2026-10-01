module Style = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub

let default_blink_speed = 0.53

type mode = Blink | Static | Hide
type msg = Tick

type t = {
  blink_speed : float;
  style : Style.t;
  text_style : Style.t;
  mode : mode;
  focused : bool;
  blinked : bool;
  hold : bool;
  char : string;
}

let v ?(blink_speed = default_blink_speed) ?(style = Style.empty)
    ?(text_style = Style.empty) () =
  {
    blink_speed = max 0.001 blink_speed;
    style;
    text_style;
    mode = Blink;
    focused = false;
    blinked = true;
    hold = false;
    char = "";
  }

let update Tick t =
  match (t.mode, t.focused) with
  | Blink, true when t.hold -> { t with blinked = false; hold = false }
  | Blink, true -> { t with blinked = not t.blinked }
  | _ -> t

let subscriptions t =
  match (t.mode, t.focused) with
  | Blink, true -> Sub.every t.blink_speed (fun _ -> Tick)
  | _ -> Sub.none

let view t =
  if t.blinked then Style.render (Style.inline true t.text_style) t.char
  else Style.render (Style.reverse true (Style.inline true t.style)) t.char

let focus t = { t with focused = true; blinked = t.mode = Hide; hold = false }
let blur t = { t with focused = false; blinked = true; hold = false }
let focused t = t.focused
let mode t = t.mode

let set_mode mode t =
  { t with mode; blinked = mode = Hide || not t.focused; hold = false }

let set_char char t = { t with char }
let set_style style t = { t with style }
let set_text_style text_style t = { t with text_style }
let blink_speed t = t.blink_speed
let set_blink_speed blink_speed t = { t with blink_speed = max 0.001 blink_speed }

let show t =
  match t.mode with
  | Hide -> { t with blinked = true; hold = false }
  | Blink | Static -> { t with blinked = false; hold = true }

let is_blinked t = t.blinked

let sync_cursor ~color ~blink ~blink_speed ~focused:desired_focused ~virtual_cursor
    ~text_style t =
  let desired_mode =
    if not virtual_cursor then Hide else if blink then Blink else Static
  in
  let c = set_style (Style.foreground color Style.empty) t in
  let c = set_text_style text_style c in
  let c = match blink_speed with Some speed -> set_blink_speed speed c | None -> c in
  let c = if mode c = desired_mode then c else set_mode desired_mode c in
  if focused c = desired_focused then c else if desired_focused then focus c else blur c
