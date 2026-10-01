module C = Charamel_lipgloss.Compositor
module Border = Charamel_lipgloss.Border
module L = Charamel_lipgloss.Layer
module P = Charamel_lipgloss.Position
module Sc = Charamel_lipgloss.Sides_color
module St = Charamel_lipgloss.Style

let oyster = Charamel_ansi.Color.Rgb (0x60, 0x5F, 0x6B)
let charple = Charamel_ansi.Color.Rgb (0x6B, 0x50, 0xFF)
let footer_text = "Press any key to swap the cards, or q to quit."
let card_a_text = "Hello"
let card_b_text = "Goodbye"

let card text =
  St.render
    St.(
      empty |> width 20 |> height 10 |> border Border.rounded
      |> border_foreground (Sc.all charple)
      |> align_horizontal P.center |> align_vertical P.center)
    text

let footer =
  St.render
    St.(empty |> height 13 |> foreground oyster |> align_vertical P.bottom)
    footer_text

let compose ~flip =
  let z_a, z_b = if flip then (1, 0) else (0, 1) in
  C.render
    (C.v
       [
         L.v footer;
         L.v ~z:z_a (card card_a_text);
         L.v ~x:10 ~y:2 ~z:z_b (card card_b_text);
       ])

type model = { flip : bool; quitting : bool }
type msg = Key of Charamel_tea.Key.t

let update msg model =
  match msg with
  | Key key -> (
      match Charamel_tea.Key.to_string key with
      | "q" | "escape" | "ctrl+c" ->
          ({ model with quitting = true }, Charamel_tea.Cmd.quit)
      | _ -> ({ model with flip = not model.flip }, Charamel_tea.Cmd.none))

let view model =
  if model.quitting then Charamel_tea.View.v ""
  else Charamel_tea.View.v (compose ~flip:model.flip)

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ flip = false; quitting = false }, Charamel_tea.Cmd.none));
    update;
    view;
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app []
    [ "│      Hel│                  │"; "│         │     Goodbye      │"; footer_text ]
  @ Smoke.expect app
      [ Smoke.key "x" ]
      [ "│      Hello       │         │"; "│                  │bye      │" ]
  @ Smoke.expect app [ Smoke.key "x"; Smoke.key "x" ] [ "│      Hel│                  │" ]
