module Progress = Charamel_bubbles.Progress
module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color

type msg = Key of Charamel_tea.Key.t | Win of int * int | Tick | Frame of Progress.msg
type model = { progress : Progress.t }

let padding = 2
let max_width = 80
let help = Style.foreground (Color.of_hex_or "#626262") Style.empty
let tick = Charamel_tea.Cmd.after 1.0 (fun () -> Tick)

let on_resize cols model =
  let wanted = cols - (padding * 2) - 4 in
  let progress = Progress.set_width (max 0 (min wanted max_width)) model.progress in
  ({ progress }, Charamel_tea.Cmd.none)

let on_tick model =
  if Progress.percent model.progress >= 1.0 then (model, Charamel_tea.Cmd.quit)
  else
    let progress = Progress.incr_percent 0.25 model.progress in
    ({ progress }, tick)

let on_frame frame model =
  let progress, cmd = Progress.update frame model.progress in
  ({ progress }, Charamel_tea.Cmd.map (fun msg -> Frame msg) cmd)

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ progress = Progress.v () }, tick));
    update =
      (fun msg model ->
        match msg with
        | Key _ -> (model, Charamel_tea.Cmd.quit)
        | Win (_rows, cols) -> on_resize cols model
        | Tick -> on_tick model
        | Frame frame -> on_frame frame model);
    view =
      (fun model ->
        Charamel_tea.View.v
          (Fmt.str "\n%s%s\n\n%s%s" (String.make padding ' ')
             (Progress.view model.progress)
             (String.make padding ' ')
             (Style.render help "Press any key to quit")));
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.resize (fun ~rows ~cols -> Win (rows, cols));
            Charamel_tea.Sub.map
              (fun msg -> Frame msg)
              (Progress.subscriptions model.progress);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [ Smoke.key "q" ] [ "   0%"; "Press any key to quit" ]
  @ Smoke.expect app [ `Wait 2.0; Smoke.key "q" ] [ " 25%" ]
  @ Smoke.expect app [ `Wait 5.0 ] [ "100%" ]
