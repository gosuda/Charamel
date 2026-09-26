module Help = Charamel_bubbles.Help
module Key_binding = Charamel_bubbles.Key_binding
module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color

let key_up = Key_binding.v [ "up"; "k" ] ~help:("↑/k", "move up")
let key_down = Key_binding.v [ "down"; "j" ] ~help:("↓/j", "move down")
let key_left = Key_binding.v [ "left"; "h" ] ~help:("←/h", "move left")
let key_right = Key_binding.v [ "right"; "l" ] ~help:("→/l", "move right")
let key_help = Key_binding.v [ "?" ] ~help:("?", "toggle help")
let key_quit = Key_binding.v [ "q"; "esc"; "ctrl+c" ] ~help:("q", "quit")

let keymap : Help.keymap =
  {
    short_help = [ key_help; key_quit ];
    full_help = [ [ key_up; key_down; key_left; key_right ]; [ key_help; key_quit ] ];
  }

let input_style = Style.empty |> Style.foreground (Color.of_hex_or "#FF75B7")
let count_newlines text = List.length (String.split_on_char '\n' text) - 1

type model = { help : Help.t; last_key : string; quitting : bool }
type msg = Key of Charamel_tea.Key.t | Win of int

let choose key =
  if Key_binding.matches key key_up then Some "↑"
  else if Key_binding.matches key key_down then Some "↓"
  else if Key_binding.matches key key_left then Some "←"
  else if Key_binding.matches key key_right then Some "→"
  else None

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ({ help = Help.v (); last_key = ""; quitting = false }, Charamel_tea.Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Win cols ->
            ({ model with help = Help.set_width cols model.help }, Charamel_tea.Cmd.none)
        | Key key -> (
            match choose key with
            | Some arrow -> ({ model with last_key = arrow }, Charamel_tea.Cmd.none)
            | None ->
                if Key_binding.matches key key_help then
                  ( {
                      model with
                      help = Help.set_show_all (not (Help.show_all model.help)) model.help;
                    },
                    Charamel_tea.Cmd.none )
                else if Key_binding.matches key key_quit then
                  ({ model with quitting = true }, Charamel_tea.Cmd.quit)
                else (model, Charamel_tea.Cmd.none)));
    view =
      (fun model ->
        if model.quitting then Charamel_tea.View.v "Bye!\n"
        else
          let status =
            if String.equal model.last_key "" then "Waiting for input..."
            else "You chose: " ^ Style.render input_style model.last_key
          in
          let help_view = Help.view model.help keymap in
          let pad = 8 - count_newlines status - count_newlines help_view in
          Charamel_tea.View.v (status ^ String.make (max 0 pad) '\n' ^ help_view));
    subscriptions =
      (fun _ ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.resize (fun ~rows:_ ~cols -> Win cols);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "Waiting for input..." ]
  @ Smoke.expect app [ Smoke.key "up" ] [ "You chose: ↑" ]
  @ Smoke.expect app [ Smoke.key "left" ] [ "You chose: ←" ]
  @ Smoke.expect app [ Smoke.key "?" ] [ "move up"; "toggle help" ]
  @ Smoke.expect app [ Smoke.key "q" ] [ "Bye!" ]
