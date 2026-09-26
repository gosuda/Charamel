module Color = Charamel_ansi.Color
module Style = Charamel_lipgloss.Style
module TextInput = Charamel_bubbles.Textinput

type slot = Foreground | Background | Cursor_color
type state = Choose | Input

let slots = [ Foreground; Background; Cursor_color ]

let slot_name = function
  | Foreground -> "Foreground"
  | Background -> "Background"
  | Cursor_color -> "Cursor"

type model = {
  ti : TextInput.t;
  choice : slot option;
  state : state;
  choice_index : int;
  err : string option;
  fg : Color.t option;
  bg : Color.t option;
  cc : Color.t option;
}

type msg = Key of Charamel_tea.Key.t | Input_msg of TextInput.msg

let none = Charamel_tea.Cmd.none

let step msg model =
  let ti, cmd = TextInput.update msg model.ti in
  ({ model with ti }, Charamel_tea.Cmd.map (fun msg -> Input_msg msg) cmd)

let choose_key key model =
  let model = { model with ti = TextInput.blur model.ti } in
  match Charamel_tea.Key.to_string key with
  | "j" | "down" -> ({ model with choice_index = (model.choice_index + 1) mod 3 }, none)
  | "k" | "up" -> ({ model with choice_index = (model.choice_index + 2) mod 3 }, none)
  | "enter" ->
      let choice = List.nth slots model.choice_index in
      let ti, cmd = TextInput.focus model.ti in
      ( { model with ti; state = Input; choice = Some choice },
        Charamel_tea.Cmd.map (fun msg -> Input_msg msg) cmd )
  | _ -> (model, none)

let apply_color choice color model =
  match choice with
  | Some Foreground -> { model with fg = Some color }
  | Some Background -> { model with bg = Some color }
  | Some Cursor_color -> { model with cc = Some color }
  | None -> model

let back_to_choice model =
  ( {
      model with
      ti = TextInput.blur model.ti;
      choice = None;
      choice_index = 0;
      state = Choose;
      err = None;
    },
    none )

let submit model =
  let value = TextInput.value model.ti in
  match Color.of_hex value with
  | None ->
      ( {
          model with
          ti = TextInput.blur model.ti;
          err = Some (Fmt.str "unable to parse color: %s" value);
        },
        none )
  | Some color -> back_to_choice (apply_color model.choice color model)

let input_key key model =
  let ti, _ = TextInput.focus model.ti in
  let model = { model with ti } in
  match Charamel_tea.Key.to_string key with
  | "escape" -> back_to_choice model
  | "enter" -> submit model
  | _ -> (
      match TextInput.key model.ti key with
      | Some msg -> step msg model
      | None -> (model, none))

let is_choose = function Choose -> true | Input -> false

let update msg model =
  match msg with
  | Key key ->
      let name = Charamel_tea.Key.to_string key in
      if String.equal name "ctrl+c" || String.equal name "q" then
        (model, Charamel_tea.Cmd.quit)
      else if is_choose model.state then choose_key key model
      else input_key key model
  | Input_msg msg -> if is_choose model.state then (model, none) else step msg model

let instructions =
  Style.render (Style.width 40 Style.empty)
    "Choose a terminal-wide color to set. All settings will be cleared on exit."

let choice_lines model =
  String.concat ""
    (List.mapi
       (fun index slot ->
         let marker = if Int.equal index model.choice_index then " > " else "   " in
         marker ^ slot_name slot ^ "\n")
       slots)

let view model =
  let head =
    match model.state with
    | Choose -> instructions ^ "\n\n" ^ choice_lines model
    | Input -> "Enter a color in hex format:\n\n" ^ TextInput.view model.ti ^ "\n"
  in
  let tail = match model.err with Some message -> "\nError: " ^ message | None -> "" in
  let footer =
    match model.state with
    | Choose -> ", j/k to move, and enter to select"
    | Input -> ", and enter to submit, esc to go back"
  in
  let cursor =
    Option.map
      (fun c ->
        {
          c with
          Charamel_tea.Cursor.row = c.Charamel_tea.Cursor.row + 2;
          color = model.cc;
        })
      (TextInput.cursor model.ti)
  in
  Charamel_tea.View.v ?cursor ?background:model.bg ?foreground:model.fg
    (head ^ tail ^ "\nPress q to quit" ^ footer ^ "\n")

let textinput_styles =
  let default = TextInput.default_styles ~is_dark:true in
  {
    default with
    Charamel_bubbles.Textinput.cursor = { default.cursor with color = Color.Indexed 63 };
  }

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( {
            ti =
              TextInput.v ~placeholder:"#ff00ff" ~char_limit:156 ~width:20
                ~virtual_cursor:false ~styles:textinput_styles ();
            choice = None;
            state = Choose;
            choice_index = 0;
            err = None;
            fg = None;
            bg = None;
            cc = None;
          },
          none ));
    update;
    view;
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map
              (fun msg -> Input_msg msg)
              (TextInput.subscriptions model.ti);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app []
    [
      "Choose a terminal-wide color to set";
      " > Foreground";
      "j/k to move, and enter to select";
    ]
  @ Smoke.expect app [ Smoke.key "j" ] [ " > Background" ]
  @ Smoke.expect app [ Smoke.key "k" ] [ " > Cursor" ]
  @ Smoke.expect app
      [ Smoke.key "enter" ]
      [ "Enter a color in hex format:"; ", and enter to submit, esc to go back" ]
  @ Smoke.expect app
      [ Smoke.key "enter"; `Text "zz"; Smoke.key "enter" ]
      [ "Error: unable to parse color: zz" ]
  @ Smoke.expect app
      [ Smoke.key "enter"; `Text "#ff00ff"; Smoke.key "enter"; Smoke.key "j" ]
      [ " > Background"; ", j/k to move, and enter to select" ]
