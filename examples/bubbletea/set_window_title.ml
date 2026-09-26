module Style = Charamel_lipgloss.Style

let window_title = "Hello, Bubble Tea"
let wrap = Style.empty |> Style.width 78

let body =
  "The window title has been set to '" ^ window_title ^ "'. It will be cleared on exit."

type msg = Key of Charamel_tea.Key.t

let view () =
  Charamel_tea.View.v ~title:window_title
    (Style.render wrap body ^ "\n\nPress any key to quit.")

let update (Key _) () = ((), Charamel_tea.Cmd.quit)
let subscriptions () = Charamel_tea.Sub.key (fun key -> Key key)
let init () = ((), Charamel_tea.Cmd.none)
let app : (unit, msg) Charamel_tea.app = { init; update; view; subscriptions }
let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "window title has been set" ]
  @ Smoke.expect app [ Smoke.key "q" ] [ "Press any key to quit." ]
