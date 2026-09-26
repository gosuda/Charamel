module Style = Charamel_lipgloss.Style
module Textinput = Charamel_bubbles.Textinput

type model = { input : Textinput.t; width : int }

type msg =
  | Key of Charamel_tea.Key.t
  | Input of Textinput.msg
  | Resize of int * int
  | Got of string option
  | Other

let route_key model key =
  match Textinput.key model.input key with
  | Some input_msg ->
      let input, cmd = Textinput.update input_msg model.input in
      ({ model with input }, Charamel_tea.Cmd.map (fun m -> Input m) cmd)
  | None -> (model, Charamel_tea.Cmd.none)

let request model name =
  let input = Textinput.reset model.input in
  ({ model with input }, Charamel_tea.Cmd.query (`Capability name))

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        let input, focus_cmd =
          Textinput.focus (Textinput.v ~placeholder:"Enter capability name to request" ())
        in
        ({ input; width = 0 }, Charamel_tea.Cmd.map (fun m -> Input m) focus_cmd));
    update =
      (fun msg model ->
        match msg with
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "ctrl+c" | "escape" -> (model, Charamel_tea.Cmd.quit)
            | "enter" -> request model (Textinput.value model.input)
            | _ -> route_key model key)
        | Input input_msg ->
            let input, cmd = Textinput.update input_msg model.input in
            ({ model with input }, Charamel_tea.Cmd.map (fun m -> Input m) cmd)
        | Resize (_, cols) -> ({ model with width = cols }, Charamel_tea.Cmd.none)
        | Got value ->
            let shown = match value with Some v -> v | None -> "" in
            (model, Charamel_tea.Cmd.print (Fmt.str "Got capability: %s" shown))
        | Other -> (model, Charamel_tea.Cmd.none));
    view =
      (fun model ->
        let instructions =
          Style.render
            Style.(empty |> width (min model.width 60))
            "Query for terminal capabilities. You can enter things like 'TN', 'RGB', \
             'cols', and so on. This will not work in all terminals and multiplexers."
        in
        Charamel_tea.View.v
          ("\n" ^ instructions ^ "\n\n" ^ Textinput.view model.input
         ^ "\n\nPress enter to request capability, or ctrl+c to quit."));
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map (fun m -> Input m) (Textinput.subscriptions model.input);
            Charamel_tea.Sub.resize (fun ~rows ~cols -> Resize (rows, cols));
            Charamel_tea.Sub.terminal (function
              | Charamel_tea.Event.Capability value -> Got value
              | _ -> Other);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app []
    [ "Query for terminal capabilities"; "Enter capability name to request" ]
  @ Smoke.expect app [ `Text "TN" ] [ "> TN" ]
  @ Smoke.expect app
      [ `Text "RGB"; Smoke.key "enter" ]
      [ "Enter capability name to request" ]
