module Mouse = Charamel_tea.Mouse

let prompt = "Do mouse stuff. When you're done press q to quit.\n"

let button_name = function
  | Mouse.Left -> "left"
  | Mouse.Middle -> "middle"
  | Mouse.Right -> "right"
  | Mouse.Wheel_up -> "wheel up"
  | Mouse.Wheel_down -> "wheel down"
  | Mouse.Wheel_left -> "wheel left"
  | Mouse.Wheel_right -> "wheel right"
  | Mouse.Backward -> "back"
  | Mouse.Forward -> "forward"
  | Mouse.Button_10 | Mouse.Button_11 -> "unknown"
  | Mouse.None_ -> ""

let describe m =
  let mods = m.Mouse.mods in
  let prefix =
    (if mods.Charamel_tea.Key.ctrl then "ctrl+" else "")
    ^ (if mods.Charamel_tea.Key.alt then "alt+" else "")
    ^ if mods.Charamel_tea.Key.shift then "shift+" else ""
  in
  Fmt.str "(X: %d, Y: %d) %s%s" m.Mouse.x m.Mouse.y prefix (button_name m.Mouse.button)

let is_quit key =
  let name = Charamel_tea.Key.to_string key in
  String.equal name "ctrl+c" || String.equal name "q" || String.equal name "escape"

type model = { lines : string list }
type msg = Key of Charamel_tea.Key.t | Mouse_event of Mouse.t

let update msg model =
  match msg with
  | Key key ->
      if is_quit key then (model, Charamel_tea.Cmd.quit)
      else (model, Charamel_tea.Cmd.none)
  | Mouse_event m -> ({ lines = model.lines @ [ describe m ] }, Charamel_tea.Cmd.none)

let view model =
  let content =
    match model.lines with
    | [] -> prompt
    | _ :: _ -> String.concat "\n" model.lines ^ "\n" ^ prompt
  in
  Charamel_tea.View.v ~mouse:Mouse_all content

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ lines = [] }, Charamel_tea.Cmd.none));
    update;
    view;
    subscriptions =
      (fun _ ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.mouse (fun m -> Mouse_event m);
          ]);
  }

let main () = Smoke.run_ app

let no_mods =
  {
    Charamel_tea.Key.shift = false;
    alt = false;
    ctrl = false;
    meta = false;
    super = false;
    hyper = false;
    caps_lock = false;
    num_lock = false;
  }

let mouse x y button = { Mouse.x; y; button; action = Mouse.Press; mods = no_mods }

let smoke () =
  Smoke.expect app [] [ "Do mouse stuff. When you're done press q to quit" ]
  @ Smoke.expect app
      [ `Msg (Mouse_event (mouse 12 4 Mouse.Left)) ]
      [ "(X: 12, Y: 4) left" ]
  @ Smoke.expect app
      [
        `Msg (Mouse_event (mouse 7 3 Mouse.Wheel_down));
        `Msg
          (Mouse_event
             { (mouse 9 2 Mouse.Right) with mods = { no_mods with ctrl = true } });
      ]
      [ "(X: 7, Y: 3) wheel down"; "(X: 9, Y: 2) ctrl+right" ]
