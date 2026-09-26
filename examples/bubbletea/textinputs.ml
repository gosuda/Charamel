module Color = Charamel_ansi.Color
module Style = Charamel_lipgloss.Style
module Textinput = Charamel_bubbles.Textinput

let focused_style = Style.(empty |> foreground (Color.Indexed 205))
let blurred_style = Style.(empty |> foreground (Color.Indexed 240))
let cursor_mode_help_style = Style.(empty |> foreground (Color.Indexed 244))

type cursor_mode = Blink | Static | Hide

let cursor_mode_name = function Blink -> "blink" | Static -> "static" | Hide -> "hide"

type model = {
  inputs : Textinput.t list;
  focus_index : int;
  cursor_mode : cursor_mode;
  quitting : bool;
}

type msg = Key of Charamel_tea.Key.t | Input of int * Textinput.msg

let input_styles () : Textinput.styles =
  let s = Textinput.default_styles ~is_dark:true in
  {
    focused = { s.focused with prompt = focused_style; text = focused_style };
    blurred = { s.blurred with prompt = blurred_style };
    cursor = { s.cursor with color = Color.Indexed 205 };
  }

let initial_inputs =
  let styles = input_styles () in
  [
    Textinput.v ~char_limit:32 ~placeholder:"Nickname" ~styles ();
    Textinput.v ~char_limit:64 ~placeholder:"Email" ~styles ();
    Textinput.v ~char_limit:32 ~echo:Textinput.Password ~echo_character:"•"
      ~placeholder:"Password" ~styles ();
  ]

let replace_nth index value xs = List.mapi (fun i x -> if i = index then value else x) xs

let set_focus index inputs =
  let inputs, cmd =
    List.fold_left
      (fun (acc, cmd) (i, t) ->
        if i = index then begin
          let t, focus_cmd = Textinput.focus t in
          let focus_cmd = Charamel_tea.Cmd.map (fun m -> Input (index, m)) focus_cmd in
          (t :: acc, Charamel_tea.Cmd.batch [ focus_cmd; cmd ])
        end
        else (Textinput.blur t :: acc, cmd))
      ([], Charamel_tea.Cmd.none)
      (List.mapi (fun i t -> (i, t)) inputs)
  in
  (List.rev inputs, cmd)

let cycle_cursor_mode model =
  let cursor_mode =
    match model.cursor_mode with Blink -> Static | Static -> Hide | Hide -> Blink
  in
  let blink = cursor_mode = Blink in
  let inputs =
    List.map
      (fun t ->
        let s = Textinput.styles t in
        Textinput.set_styles { s with cursor = { s.cursor with blink } } t)
      model.inputs
  in
  ({ model with cursor_mode; inputs }, Charamel_tea.Cmd.none)

let route_key model key =
  let inputs, cmd =
    List.fold_left
      (fun (acc, cmd) (i, t) ->
        match Textinput.key t key with
        | Some input_msg ->
            let t, update_cmd = Textinput.update input_msg t in
            let update_cmd = Charamel_tea.Cmd.map (fun m -> Input (i, m)) update_cmd in
            (t :: acc, Charamel_tea.Cmd.batch [ update_cmd; cmd ])
        | None -> (t :: acc, cmd))
      ([], Charamel_tea.Cmd.none)
      (List.mapi (fun i t -> (i, t)) model.inputs)
  in
  ({ model with inputs = List.rev inputs }, cmd)

let move_focus model name =
  let n = List.length model.inputs in
  let delta = if name = "up" || name = "shift+tab" then -1 else 1 in
  let focus_index =
    let i = model.focus_index + delta in
    if i > n then 0 else if i < 0 then n else i
  in
  let inputs, cmd = set_focus focus_index model.inputs in
  ({ model with focus_index; inputs }, cmd)

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        let inputs, cmd = set_focus 0 initial_inputs in
        ({ inputs; focus_index = 0; cursor_mode = Blink; quitting = false }, cmd));
    update =
      (fun msg model ->
        match msg with
        | Input (index, input_msg) ->
            let t, cmd = Textinput.update input_msg (List.nth model.inputs index) in
            ( { model with inputs = replace_nth index t model.inputs },
              Charamel_tea.Cmd.map (fun m -> Input (index, m)) cmd )
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "ctrl+c" | "escape" ->
                ({ model with quitting = true }, Charamel_tea.Cmd.quit)
            | "ctrl+r" -> cycle_cursor_mode model
            | ("tab" | "shift+tab" | "enter" | "up" | "down") as name ->
                if name = "enter" && model.focus_index = List.length model.inputs then
                  (model, Charamel_tea.Cmd.quit)
                else move_focus model name
            | _ -> route_key model key));
    view =
      (fun model ->
        let n = List.length model.inputs in
        let text = String.concat "\n" (List.map Textinput.view model.inputs) in
        let button =
          if model.focus_index = n then Style.render focused_style "[ Submit ]"
          else "[ " ^ Style.render blurred_style "Submit" ^ " ]"
        in
        let help =
          Style.render blurred_style "cursor mode is "
          ^ Style.render cursor_mode_help_style (cursor_mode_name model.cursor_mode)
          ^ Style.render blurred_style " (ctrl+r to change style)"
        in
        let content = text ^ "\n\n" ^ button ^ "\n\n" ^ help in
        let content = if model.quitting then content ^ "\n" else content in
        let cursor =
          if model.cursor_mode = Hide || model.focus_index >= n then None
          else
            match Textinput.cursor (List.nth model.inputs model.focus_index) with
            | Some c ->
                Some { c with Charamel_tea.Cursor.row = c.row + model.focus_index }
            | None -> None
        in
        Charamel_tea.View.v ?cursor content);
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          (Charamel_tea.Sub.key (fun key -> Key key)
          :: List.mapi
               (fun i t ->
                 Charamel_tea.Sub.map (fun m -> Input (i, m)) (Textinput.subscriptions t))
               model.inputs));
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "Nickname"; "cursor mode is " ]
  @ Smoke.expect app [ `Text "Neo" ] [ "Neo" ]
  @ Smoke.expect app
      [ `Text "Neo"; Smoke.key "tab"; `Text "neo@x.io" ]
      [ "Neo"; "neo@x.io" ]
  @ Smoke.expect app [ Smoke.key "tab"; Smoke.key "tab"; `Text "hunter2" ] [ "•••••••" ]
  @ Smoke.expect app [ Smoke.key "tab"; Smoke.key "shift+tab" ] [ "Nickname" ]
