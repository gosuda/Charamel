module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color

let fancy_color = Color.of_hex_or "#6b50ff"
let fancy_style = Style.empty |> Style.foreground fancy_color

type msg = Key of Charamel_tea.Key.t

let app : (unit, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( (),
          Charamel_tea.Cmd.batch
            [
              Charamel_tea.Cmd.query (`Capability "RGB");
              Charamel_tea.Cmd.query (`Capability "Tc");
            ] ));
    update = (fun (Key _) () -> ((), Charamel_tea.Cmd.quit));
    view =
      (fun () ->
        Charamel_tea.View.v
          ("This will produce the wrong colors on Apple Terminal :)\n\n"
          ^ Style.render fancy_style "Howdy!"
          ^ "\n\nPress any key to exit."));
    subscriptions = (fun () -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let main () =
  Lwt.map (fun _ -> ()) (Smoke.run ~color_profile:Charamel_colorprofile.True_color app)

let smoke () =
  Smoke.expect app [] [ "wrong colors on Apple Terminal"; "Howdy!" ]
  @ Smoke.expect app [ Smoke.key "q" ] [ "Press any key to exit." ]
