module Cmd = Charamel_tea.Cmd
module Color = Charamel_ansi.Color
module Key = Charamel_tea.Key
module Spin = Charamel_bubbles.Spinner
module Style = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View

let spinners =
  [
    Spin.Line;
    Spin.Dot;
    Spin.Mini_dot;
    Spin.Jump;
    Spin.Pulse;
    Spin.Points;
    Spin.Globe;
    Spin.Moon;
    Spin.Monkey;
  ]

let text_style = Style.(empty |> foreground (Color.Indexed 252))
let spinner_style = Style.(empty |> foreground (Color.Indexed 69))
let help_style = Style.(empty |> foreground (Color.Indexed 241))

type model = { index : int; spinner : Spin.t }
type msg = Key of Key.t | Spin of Spin.msg

let reset index =
  { index; spinner = Spin.v ~kind:(List.nth spinners index) ~style:spinner_style () }

let shift model delta =
  reset ((model.index + delta + List.length spinners) mod List.length spinners)

let on_key key model =
  match Key.to_string key with
  | "ctrl+c" | "q" | "escape" -> (model, Cmd.quit)
  | "h" | "left" -> (shift model (-1), Cmd.none)
  | "l" | "right" -> (shift model 1, Cmd.none)
  | _ -> (model, Cmd.none)

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> (reset 0, Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Key key -> on_key key model
        | Spin spin_msg ->
            let spinner, cmd = Spin.update spin_msg model.spinner in
            ({ model with spinner }, Cmd.map (fun msg -> Spin msg) cmd));
    view =
      (fun model ->
        let gap = if model.index = 1 then "" else " " in
        View.v
          ("\n " ^ Spin.view model.spinner ^ gap
          ^ Style.render text_style "Spinning..."
          ^ "\n\n"
          ^ Style.render help_style "h/l, ←/→: change spinner • q: exit\n"));
    subscriptions =
      (fun model ->
        Sub.batch
          [
            Sub.key (fun key -> Key key);
            Sub.map (fun msg -> Spin msg) (Spin.subscriptions model.spinner);
          ]);
  }

let main () = Smoke.run_ app
let frame kind offset = List.nth (Spin.frames kind) offset

let smoke () =
  Smoke.expect app []
    [ frame Spin.Line 0 ^ " Spinning..."; "h/l, ←/→: change spinner • q: exit" ]
  @ Smoke.expect app [ Smoke.key "l" ] [ frame Spin.Dot 0 ^ "Spinning..." ]
  @ Smoke.expect app [ Smoke.key "h" ] [ frame Spin.Monkey 0 ^ " Spinning..." ]
  @ Smoke.expect app [ `Wait 0.1 ] [ frame Spin.Line 1 ^ " Spinning..." ]
  @ Smoke.expect app
      [ Smoke.key "l"; Smoke.key "l" ]
      [ frame Spin.Mini_dot 0 ^ " Spinning..." ]
