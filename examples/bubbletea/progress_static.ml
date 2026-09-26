module Progress = Charamel_bubbles.Progress
module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color

type msg = Key of Charamel_tea.Key.t | Win of int * int | Tick
type model = { percent : float; progress : Progress.t }

let padding = 2
let max_width = 80
let help = Style.foreground (Color.of_hex_or "#626262") Style.empty
let pink = Color.of_hex_or "#FF7CCB"
let yellow = Color.of_hex_or "#FDFF8C"
let tick = Charamel_tea.Cmd.after 1.0 (fun () -> Tick)

let fit_width cols progress =
  let wanted = cols - (padding * 2) - 4 in
  Progress.set_width (max 0 (min wanted max_width)) progress

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( { percent = 0.0; progress = Progress.v ~scaled:true ~colors:[ pink; yellow ] () },
          tick ));
    update =
      (fun msg model ->
        match msg with
        | Key _ -> (model, Charamel_tea.Cmd.quit)
        | Win (_rows, cols) ->
            ( { model with progress = fit_width cols model.progress },
              Charamel_tea.Cmd.none )
        | Tick ->
            let percent = model.percent +. 0.25 in
            if percent > 1.0 then ({ model with percent = 1.0 }, Charamel_tea.Cmd.quit)
            else ({ model with percent }, tick));
    view =
      (fun model ->
        Charamel_tea.View.v
          (Fmt.str "\n%s%s\n\n%s%s" (String.make padding ' ')
             (Progress.view_as model.percent model.progress)
             (String.make padding ' ')
             (Style.render help "Press any key to quit")));
    subscriptions =
      (fun _ ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.resize (fun ~rows ~cols -> Win (rows, cols));
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [ Smoke.key "q" ] [ "   0%"; "Press any key to quit" ]
  @ Smoke.expect app [ `Wait 1.5; Smoke.key "q" ] [ " 25%" ]
  @ Smoke.expect app [ `Wait 3.5; Smoke.key "q" ] [ " 75%" ]
  @ Smoke.expect app [ `Wait 4.5 ] [ "100%" ]
