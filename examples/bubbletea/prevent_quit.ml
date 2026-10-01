module Textarea = Charamel_bubbles.Textarea
module Help = Charamel_bubbles.Help
module Key_binding = Charamel_bubbles.Key_binding
module Style = Charamel_lipgloss.Style
module Sides = Charamel_lipgloss.Sides
module Sides_color = Charamel_lipgloss.Sides_color
module Border = Charamel_lipgloss.Border
module Layout = Charamel_lipgloss.Layout
module Color = Charamel_ansi.Color

let key_save = Key_binding.v [ "ctrl+s" ] ~help:("ctrl+s", "save")
let key_quit = Key_binding.v [ "esc"; "ctrl+c" ] ~help:("esc", "quit")

let choice_style =
  Style.empty |> Style.padding_side `Left 1 |> Style.foreground (Color.Indexed 241)

let save_text_style = Style.empty |> Style.foreground (Color.Indexed 170)

let quit_view_style =
  Style.empty
  |> Style.padding (Sides.xy ~x:3 ~y:1)
  |> Style.border Border.rounded
  |> Style.border_foreground (Sides_color.all (Color.Indexed 170))

let initial_area = Textarea.v ~placeholder:"Only the best words" ()

type model = {
  area : Textarea.t;
  help : Help.t;
  save_text : string;
  has_changes : bool;
  quitting : bool;
}

type msg = Key of Charamel_tea.Key.t | Area of Textarea.msg | Quit

let area_cmd cmd = Charamel_tea.Cmd.map (fun msg -> Area msg) cmd

let route_key key model =
  let model, focus =
    if Textarea.focused model.area then (model, Charamel_tea.Cmd.none)
    else
      let area, cmd = Textarea.focus model.area in
      ({ model with area }, area_cmd cmd)
  in
  match Textarea.key model.area key with
  | Some msg ->
      let area, cmd = Textarea.update msg model.area in
      ({ model with area }, Charamel_tea.Cmd.batch [ focus; area_cmd cmd ])
  | None -> (model, focus)

let update_text msg model =
  match msg with
  | Key key ->
      let model = { model with save_text = "" } in
      if Key_binding.matches key key_save then
        let model = { model with save_text = "Changes saved!"; has_changes = false } in
        route_key key model
      else if Key_binding.matches key key_quit then
        ({ model with quitting = true }, Charamel_tea.Cmd.msg Quit)
      else
        let model =
          if String.equal key.text "" then model else { model with has_changes = true }
        in
        route_key key model
  | Area msg ->
      let area, cmd = Textarea.update msg model.area in
      ({ model with area }, area_cmd cmd)
  | Quit -> (model, Charamel_tea.Cmd.none)

let update_prompt msg model =
  match msg with
  | Key key ->
      if
        Key_binding.matches key key_quit
        || String.equal (Charamel_tea.Key.to_string key) "y"
      then ({ model with has_changes = false }, Charamel_tea.Cmd.quit)
      else ({ model with quitting = false }, Charamel_tea.Cmd.none)
  | Area _ | Quit -> (model, Charamel_tea.Cmd.none)

let accept model msg = match msg with Quit when model.has_changes -> false | _ -> true

let update msg model =
  match accept model msg with
  | false -> (model, Charamel_tea.Cmd.none)
  | true -> (
      match msg with
      | Quit -> (model, Charamel_tea.Cmd.quit)
      | _ when model.quitting -> update_prompt msg model
      | _ -> update_text msg model)

let prompt_text = "You have unsaved changes. Quit without saving?"

let view model =
  if not model.quitting then
    let help_view = Help.short_view model.help [ key_save; key_quit ] in
    Charamel_tea.View.v
      (Fmt.str "Type some important things.\n%s\n %s\n %s" (Textarea.view model.area)
         (Style.render save_text_style model.save_text)
         help_view
      ^ "\n\n")
  else if not model.has_changes then Charamel_tea.View.v "Very important. Thank you.\n"
  else
    let text = Layout.join_horizontal [ prompt_text; Style.render choice_style "[yN]" ] in
    Charamel_tea.View.v (Style.render quit_view_style text)

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        let area, cmd = Textarea.focus initial_area in
        ( { area; help = Help.v (); save_text = ""; has_changes = false; quitting = false },
          area_cmd cmd ));
    update;
    view;
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map (fun msg -> Area msg) (Textarea.subscriptions model.area);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "Type some important things."; "Only the best words" ]
  @ Smoke.expect app [ `Text "hello"; Smoke.key "esc" ] [ prompt_text; "[yN]" ]
  @ Smoke.expect app
      [ `Text "hello"; Smoke.key "esc"; Smoke.key "n" ]
      [ "Type some important things."; "hello" ]
  @ Smoke.expect app
      [ `Text "hello"; Smoke.key "esc"; Smoke.key "y" ]
      [ "Very important. Thank you." ]
  @ Smoke.expect app [ Smoke.key "esc" ] [ "Very important. Thank you." ]
  @ Smoke.expect app [ `Text "hello"; Smoke.key "ctrl+s" ] [ "Changes saved!" ]
