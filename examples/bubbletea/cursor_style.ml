module Cursor = Charamel_tea.Cursor

type model = { shape : int; blink : bool }
type msg = Key of Charamel_tea.Key.t

let shapes = [ Cursor.Block; Cursor.Underline; Cursor.Bar ]
let cursor_shape model = List.nth shapes model.shape

let describe model =
  let adjective = if model.blink then "blinking" else "steady" in
  let noun =
    match cursor_shape model with
    | Cursor.Block -> "block"
    | Cursor.Underline -> "underline"
    | Cursor.Bar -> "bar"
  in
  Fmt.str "%s %s" adjective noun

let cycle model step = { shape = (model.shape + step + 3) mod 3; blink = not model.blink }

let update msg model =
  match msg with
  | Key key -> (
      match Charamel_tea.Key.to_string key with
      | "q" | "ctrl+c" -> (model, Charamel_tea.Cmd.quit)
      | "h" | "left" -> (cycle model (-1), Charamel_tea.Cmd.none)
      | "l" | "right" -> (cycle model 1, Charamel_tea.Cmd.none)
      | _ -> ({ shape = model.shape; blink = not model.blink }, Charamel_tea.Cmd.none))

let view model =
  Charamel_tea.View.v
    ~cursor:(Cursor.v ~shape:(cursor_shape model) ~blink:model.blink 0 2)
    ("Press left/right to change the cursor style, q or ctrl+c to quit.\n\n\
      <- This is the cursor (a " ^ describe model ^ ")")

let subscriptions _ = Charamel_tea.Sub.key (fun key -> Key key)
let init () = ({ shape = 0; blink = true }, Charamel_tea.Cmd.none)
let app : (model, msg) Charamel_tea.app = { init; update; view; subscriptions }
let main () = Smoke.run_ app
let cursor_state events = Smoke.expect app events

let smoke () =
  cursor_state [] [ "cursor (a blinking block)" ]
  @ cursor_state [ Smoke.key "l" ] [ "cursor (a steady underline)" ]
  @ cursor_state [ Smoke.key "h"; Smoke.key "h" ] [ "cursor (a blinking underline)" ]
  @ cursor_state [ Smoke.key "l"; Smoke.key "l" ] [ "cursor (a blinking bar)" ]
  @ cursor_state [ Smoke.key "up" ] [ "cursor (a steady block)" ]
