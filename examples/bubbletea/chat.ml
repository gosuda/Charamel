module Area = Charamel_bubbles.Textarea
module Cmd = Charamel_tea.Cmd
module Color = Charamel_ansi.Color
module Key = Charamel_tea.Key
module Key_binding = Charamel_bubbles.Key_binding
module Pane = Charamel_bubbles.Viewport
module Style = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View

type model = {
  viewport : Pane.t;
  messages : string list;
  textarea : Area.t;
  sender_style : Style.t;
}

type msg = Key of Key.t | Area of Area.msg | Win of { rows : int; cols : int }

let welcome = "Welcome to the chat room!\nType a message and press Enter to send."

let keymap =
  let km = Area.default_keymap in
  { km with insert_newline = Key_binding.set_enabled false km.insert_newline }

let styles =
  let s = Area.default_styles ~is_dark:true in
  { s with focused = { s.focused with cursor_line = Style.empty } }

let viewport_keymap =
  let km = Pane.default_keymap in
  {
    km with
    left = Key_binding.set_enabled false km.left;
    right = Key_binding.set_enabled false km.right;
  }

let new_model () =
  let textarea =
    Area.v ~placeholder:"Send a message..." ~show_line_numbers:false ~char_limit:280
      ~width:30 ~height:3 ~virtual_cursor:false ~keymap ~styles ()
  in
  let viewport =
    Pane.v ~width:30 ~height:5 ~keymap:viewport_keymap () |> Pane.set_content welcome
  in
  {
    viewport;
    messages = [];
    textarea;
    sender_style = Style.(empty |> foreground (Color.Indexed 5));
  }

let wrapped viewport messages =
  Style.render Style.(empty |> width (Pane.width viewport)) (String.concat "\n" messages)

let apply_area msg model =
  let textarea, cmd = Area.update msg model.textarea in
  ({ model with textarea }, Cmd.map (fun msg -> Area msg) cmd)

let on_enter model =
  let message = Style.render model.sender_style "You: " ^ Area.value model.textarea in
  let messages = model.messages @ [ message ] in
  let viewport =
    model.viewport
    |> Pane.set_content (wrapped model.viewport messages)
    |> Pane.goto_bottom
  in
  ({ model with messages; viewport; textarea = Area.reset model.textarea }, Cmd.none)

let on_key key model =
  match Key.to_string key with
  | "ctrl+c" | "escape" ->
      (model, Cmd.seq [ Cmd.print (Area.value model.textarea); Cmd.quit ])
  | "enter" -> on_enter model
  | _ -> (
      match Area.key model.textarea key with
      | Some msg -> apply_area msg model
      | None -> (model, Cmd.none))

let on_win rows cols model =
  let textarea = Area.set_width cols model.textarea in
  let viewport =
    model.viewport |> Pane.set_width cols |> Pane.set_height (rows - Area.height textarea)
  in
  let viewport =
    if model.messages = [] then viewport
    else Pane.set_content (wrapped viewport model.messages) viewport
  in
  ({ model with textarea; viewport = Pane.goto_bottom viewport }, Cmd.none)

let line_count text = List.length (String.split_on_char '\n' text)

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        let model = new_model () in
        let textarea, cmd = Area.focus model.textarea in
        ({ model with textarea }, Cmd.map (fun area_msg -> Area area_msg) cmd));
    update =
      (fun msg model ->
        match msg with
        | Key key -> on_key key model
        | Area area_msg -> apply_area area_msg model
        | Win { rows; cols } -> on_win rows cols model);
    view =
      (fun model ->
        let viewport_view = Pane.view model.viewport in
        let cursor =
          match Area.cursor model.textarea with
          | Some c -> Some { c with row = c.row + line_count viewport_view }
          | None -> None
        in
        View.v ~alt_screen:true ?cursor (viewport_view ^ "\n" ^ Area.view model.textarea));
    subscriptions =
      (fun model ->
        Sub.batch
          [
            Sub.key (fun key -> Key key);
            Sub.resize (fun ~rows ~cols -> Win { rows; cols });
            Sub.map (fun msg -> Area msg) (Area.subscriptions model.textarea);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app []
    [
      "Welcome to the chat room!";
      "Type a message and press Enter to send.";
      "Send a message...";
    ]
  @ Smoke.expect app
      [ `Text "hello"; Smoke.key "enter" ]
      [ "You: hello"; "Send a message..." ]
  @ Smoke.expect app
      [ `Text "hi"; Smoke.key "enter"; `Text "there"; Smoke.key "enter" ]
      [ "You: hi"; "You: there" ]
  @ Smoke.expect app [ `Text "typing" ] [ "typing" ]
