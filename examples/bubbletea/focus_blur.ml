type model = { focused : bool; reporting : bool }
type msg = Key of Charamel_tea.Key.t | Focus of [ `Focused | `Blurred ]

let status model =
  let label = if model.reporting then "enabled" else "disabled" in
  let s = "Hi. Focus report is currently " ^ label ^ ".\n\n" in
  if not model.reporting then s
  else
    s
    ^
    if model.focused then "This program is currently focused!"
    else "This program is currently blurred!"

let update msg model =
  match msg with
  | Focus `Focused -> ({ model with focused = true }, Charamel_tea.Cmd.none)
  | Focus `Blurred -> ({ model with focused = false }, Charamel_tea.Cmd.none)
  | Key key -> (
      match Charamel_tea.Key.to_string key with
      | "t" -> ({ model with reporting = not model.reporting }, Charamel_tea.Cmd.none)
      | "q" | "ctrl+c" -> (model, Charamel_tea.Cmd.quit)
      | _ -> (model, Charamel_tea.Cmd.none))

let view model =
  Charamel_tea.View.v ~report_focus:model.reporting
    (status model ^ "\n\nTo quit sooner press ctrl-c, or t to toggle focus reporting...\n")

let subscriptions _ =
  Charamel_tea.Sub.batch
    [
      Charamel_tea.Sub.key (fun key -> Key key);
      Charamel_tea.Sub.focus (fun state -> Focus state);
    ]

let init () = ({ focused = true; reporting = true }, Charamel_tea.Cmd.none)
let app : (model, msg) Charamel_tea.app = { init; update; view; subscriptions }
let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "Focus report is currently enabled"; "currently focused!" ]
  @ Smoke.expect app [ `Msg (Focus `Blurred) ] [ "currently blurred!" ]
  @ Smoke.expect app
      [ `Msg (Focus `Blurred); Smoke.key "t" ]
      [ "Focus report is currently disabled" ]
  @ Smoke.expect app [ Smoke.key "t" ] [ "t to toggle focus reporting" ]
