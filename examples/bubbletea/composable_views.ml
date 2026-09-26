module Spinner = Charamel_bubbles.Spinner
module Timer = Charamel_bubbles.Timer
module Style = Charamel_lipgloss.Style
module Border = Charamel_lipgloss.Border
module Sides_color = Charamel_lipgloss.Sides_color
module Layout = Charamel_lipgloss.Layout
module Position = Charamel_lipgloss.Position
module Color = Charamel_ansi.Color

let default_time = 60.0

let spinners =
  [
    Spinner.Line;
    Spinner.Dot;
    Spinner.Mini_dot;
    Spinner.Jump;
    Spinner.Pulse;
    Spinner.Points;
    Spinner.Globe;
    Spinner.Moon;
    Spinner.Monkey;
  ]

let box =
  Style.empty |> Style.width 15 |> Style.height 5
  |> Style.align_horizontal Position.center
  |> Style.align_vertical Position.center

let model_style = box |> Style.border Border.hidden

let focused_model_style =
  box |> Style.border Border.normal
  |> Style.border_foreground (Sides_color.all (Color.Indexed 69))

let spinner_style = Style.empty |> Style.foreground (Color.Indexed 69)
let help_style = Style.empty |> Style.foreground (Color.Indexed 241)
let new_spinner index = Spinner.v ~kind:(List.nth spinners index) ~style:spinner_style ()

type state = Timer_view | Spinner_view
type model = { state : state; timer : Timer.t; spinner : Spinner.t; index : int }
type msg = Key of Charamel_tea.Key.t | Spin of Spinner.msg | Timer_msg of Timer.msg

let initial_model =
  {
    state = Timer_view;
    timer = Timer.v ~timeout:default_time ();
    spinner = Spinner.v ~kind:Spinner.Dot ();
    index = 0;
  }

let new_focus model =
  match model.state with
  | Timer_view -> { model with timer = Timer.v ~timeout:default_time () }
  | Spinner_view ->
      let index = (model.index + 1) mod List.length spinners in
      { model with index; spinner = new_spinner index }

let update msg model =
  match msg with
  | Spin msg ->
      let spinner, cmd = Spinner.update msg model.spinner in
      ({ model with spinner }, Charamel_tea.Cmd.map (fun msg -> Spin msg) cmd)
  | Timer_msg msg ->
      let timer, cmd = Timer.update msg model.timer in
      ({ model with timer }, Charamel_tea.Cmd.map (fun msg -> Timer_msg msg) cmd)
  | Key key -> (
      match Charamel_tea.Key.to_string key with
      | "q" | "ctrl+c" -> (model, Charamel_tea.Cmd.quit)
      | "tab" ->
          let state =
            match model.state with
            | Timer_view -> Spinner_view
            | Spinner_view -> Timer_view
          in
          ({ model with state }, Charamel_tea.Cmd.none)
      | "n" -> (new_focus model, Charamel_tea.Cmd.none)
      | _ -> (model, Charamel_tea.Cmd.none))

let focused_name model =
  match model.state with Timer_view -> "timer" | Spinner_view -> "spinner"

let view model =
  let timer_text = Fmt.str "%4s" (Timer.view model.timer) in
  let spinner_text = Spinner.view model.spinner in
  let blocks =
    match model.state with
    | Timer_view ->
        [
          Style.render focused_model_style timer_text;
          Style.render model_style spinner_text;
        ]
    | Spinner_view ->
        [
          Style.render model_style timer_text;
          Style.render focused_model_style spinner_text;
        ]
  in
  let help_text =
    Fmt.str "\ntab: focus next • n: new %s • q: exit\n" (focused_name model)
  in
  Charamel_tea.View.v (Layout.join_horizontal blocks ^ Style.render help_style help_text)

let subscriptions model =
  Charamel_tea.Sub.batch
    [
      Charamel_tea.Sub.key (fun key -> Key key);
      Charamel_tea.Sub.map (fun msg -> Spin msg) (Spinner.subscriptions model.spinner);
      Charamel_tea.Sub.map (fun msg -> Timer_msg msg) (Timer.subscriptions model.timer);
    ]

let init () = (initial_model, Charamel_tea.Cmd.none)
let app : (model, msg) Charamel_tea.app = { init; update; view; subscriptions }
let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "1m0s"; "n: new timer"; "tab: focus next" ]
  @ Smoke.expect app [ `Wait 1.0 ] [ "59s" ]
  @ Smoke.expect app [ Smoke.key "tab" ] [ "n: new spinner" ]
  @ Smoke.expect app [ Smoke.key "tab"; Smoke.key "n"; Smoke.key "n" ] [ "⠋" ]
  @ Smoke.expect app [ `Wait 2.0; Smoke.key "n" ] [ "1m0s" ]
