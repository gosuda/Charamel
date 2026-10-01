module Style = Charamel_lipgloss.Style
module Sides = Charamel_lipgloss.Sides
module View = Charamel_tea.View

let body_text =
  "This demo requires a terminal emulator that supports an indeterminate progress bar, \
   such a Windows Terminal or Ghostty. In other terminals (including tmux in a \
   supporting terminal) nothing will happen.\n\n\
   Press up/down to change value, left/right to change state, q to quit."

let padding_cells = 4

type model = { value : int; width : int; state : int }
type msg = Key of Charamel_tea.Key.t | Win of int

let progress state value =
  match state with
  | 0 -> View.Progress_none
  | 1 -> View.Progress_value value
  | 2 -> View.Progress_error value
  | 3 -> View.Progress_indeterminate
  | _ -> View.Progress_warning value

let body model =
  Style.empty
  |> Style.padding (Sides.xy ~x:2 ~y:1)
  |> Style.width (model.width - padding_cells)

let move model name =
  match name with
  | "up" | "k" -> { model with value = min 100 (model.value + 10) }
  | "down" | "j" -> { model with value = max 0 (model.value - 10) }
  | "left" | "h" -> { model with state = max 0 (model.state - 1) }
  | "right" | "l" -> { model with state = min 4 (model.state + 1) }
  | _ -> model

let update msg model =
  match msg with
  | Win cols -> ({ model with width = cols }, Charamel_tea.Cmd.none)
  | Key key ->
      let name = Charamel_tea.Key.to_string key in
      if String.equal name "q" || String.equal name "ctrl+c" then
        (model, Charamel_tea.Cmd.quit)
      else (move model name, Charamel_tea.Cmd.none)

let view model =
  Charamel_tea.View.v
    ~progress:(progress model.state model.value)
    (Style.render (body model) body_text)

let subscriptions _ =
  Charamel_tea.Sub.batch
    [
      Charamel_tea.Sub.key (fun key -> Key key);
      Charamel_tea.Sub.resize (fun ~rows:_ ~cols -> Win cols);
    ]

let init () = ({ value = 50; width = 0; state = 3 }, Charamel_tea.Cmd.none)
let app : (model, msg) Charamel_tea.app = { init; update; view; subscriptions }
let main () = Smoke.run_ app
let wide_line = "This demo requires a terminal emulator that supports an indeterminate"
let narrow_line = "an indeterminate progress bar, such a Windows"

let smoke () =
  Smoke.expect app [] [ wide_line ]
  @ Smoke.expect app
      [ `Resize (24, 60); Smoke.key "up"; Smoke.key "right" ]
      [ narrow_line ]
