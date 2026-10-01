type model = { altscreen : bool; quitting : bool; suspending : bool }
type msg = Key of Charamel_tea.Key.t | Resume

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( { altscreen = false; quitting = false; suspending = false },
          Charamel_tea.Cmd.none ));
    update =
      (fun msg model ->
        match msg with
        | Resume -> ({ model with suspending = false }, Charamel_tea.Cmd.none)
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "q" | "ctrl+c" | "escape" ->
                ({ model with quitting = true }, Charamel_tea.Cmd.quit)
            | "ctrl+z" -> ({ model with suspending = true }, Charamel_tea.Cmd.suspend)
            | "space" ->
                ({ model with altscreen = not model.altscreen }, Charamel_tea.Cmd.none)
            | _ -> (model, Charamel_tea.Cmd.none)));
    view =
      (fun model ->
        let content =
          if model.suspending then ""
          else if model.quitting then "Bye!\n"
          else
            let mode = if model.altscreen then " altscreen mode " else " inline mode " in
            let keyword =
              Charamel_lipgloss.Style.(
                render (empty |> foreground (Indexed 204) |> background (Indexed 235)))
                mode
            in
            let help =
              Charamel_lipgloss.Style.(render (empty |> foreground (Indexed 241)))
                "  space: switch modes • ctrl-z: suspend • q: exit\n"
            in
            "\n\n  You're in " ^ keyword ^ "\n\n\n" ^ help
        in
        Charamel_tea.View.v ~alt_screen:model.altscreen content);
    subscriptions =
      (fun _ ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.resume (fun () -> Resume);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "inline mode" ]
  @ Smoke.expect app [ Smoke.key "space" ] [ "altscreen mode" ]
  @ Smoke.expect app [ Smoke.key "space"; Smoke.key "space" ] [ "inline mode" ]
  @ Smoke.expect app [ Smoke.key "q" ] [ "Bye!" ]
  @ Smoke.expect app [ `Msg Resume ] [ "You're in" ]
