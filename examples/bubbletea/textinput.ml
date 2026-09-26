module Textinput = Charamel_bubbles.Textinput
module Layout = Charamel_lipgloss.Layout

let header_view = "What’s your favorite Pokémon?\n"
let footer_view = "\n(esc to quit)"

let initial_input =
  Textinput.v ~placeholder:"Pikachu" ~virtual_cursor:false ~char_limit:156 ~width:20 ()

type model = { input : Textinput.t; quitting : bool }
type msg = Key of Charamel_tea.Key.t | Input of Textinput.msg

let input_cmd cmd = Charamel_tea.Cmd.map (fun msg -> Input msg) cmd

let edit msg model =
  let input, cmd = Textinput.update msg model.input in
  ({ model with input }, input_cmd cmd)

let is_quit_key name =
  String.equal name "enter" || String.equal name "ctrl+c" || String.equal name "esc"

let update msg model =
  match msg with
  | Input msg -> edit msg model
  | Key key -> (
      if is_quit_key (Charamel_tea.Key.to_string key) then
        ({ model with quitting = true }, Charamel_tea.Cmd.quit)
      else
        match Textinput.key model.input key with
        | Some msg -> edit msg model
        | None -> (model, Charamel_tea.Cmd.none))

let view model =
  let content =
    Layout.join_vertical [ header_view; Textinput.view model.input; footer_view ]
  in
  let content = if model.quitting then content ^ "\n" else content in
  let cursor =
    match Textinput.cursor model.input with
    | Some cursor ->
        Some
          { cursor with Charamel_tea.Cursor.row = cursor.row + Layout.height header_view }
    | None -> None
  in
  Charamel_tea.View.v ?cursor content

let subscriptions model =
  Charamel_tea.Sub.batch
    [
      Charamel_tea.Sub.key (fun key -> Key key);
      Charamel_tea.Sub.map (fun msg -> Input msg) (Textinput.subscriptions model.input);
    ]

let init () =
  let input, cmd = Textinput.focus initial_input in
  ({ input; quitting = false }, input_cmd cmd)

let app : (model, msg) Charamel_tea.app = { init; update; view; subscriptions }
let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app
    [ Smoke.key "esc" ]
    [ "What’s your favorite Pokémon?"; "Pikachu"; "(esc to quit)" ]
  @ Smoke.expect app [ `Text "Ash"; Smoke.key "esc" ] [ "Ash" ]
  @ Smoke.expect app [ `Text "Bulbasaur"; Smoke.key "enter" ] [ "Bulbasaur" ]
  @ Smoke.expect app
      [ `Text "hello world"; Smoke.key "backspace"; Smoke.key "esc" ]
      [ "hello worl" ]
