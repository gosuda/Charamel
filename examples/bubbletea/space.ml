module Color = Charamel_ansi.Color
module Style = Charamel_lipgloss.Style

let tick_interval = 1.0 /. 60.0

type model = {
  width : int;
  height : int;
  frame_count : int;
  colors : Color.t array array;
}

type msg = Tick | Key of Charamel_tea.Key.t | Resize of int * int

let clamp value low high = min (max value low) high
let tick = Charamel_tea.Cmd.after tick_interval (fun () -> Tick)

let setup_colors width height =
  let rows = height * 2 in
  Array.init rows (fun y ->
      let factor = float_of_int (rows - y) /. float_of_int rows in
      Array.init width (fun _ ->
          let value = clamp ((factor *. factor) +. (Random.float 0.2 -. 0.1)) 0. 1. in
          let gray = int_of_float (value *. 255.) in
          Color.Rgb (gray, gray, gray)))

let resize rows cols model =
  if model.width = cols && model.height = rows then model
  else { model with width = cols; height = rows; colors = setup_colors cols rows }

let cell model y x =
  let xi = (x + model.frame_count) mod model.width in
  let fg = model.colors.(y * 2).(xi) in
  let bg = model.colors.((y * 2) + 1).(xi) in
  Style.render Style.(empty |> foreground fg |> background bg) "▀"

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ width = 0; height = 0; frame_count = 0; colors = [||] }, tick));
    update =
      (fun msg model ->
        match msg with
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "q" | "ctrl+c" -> (model, Charamel_tea.Cmd.quit)
            | _ -> (model, Charamel_tea.Cmd.none))
        | Resize (rows, cols) -> (resize rows cols model, Charamel_tea.Cmd.none)
        | Tick -> ({ model with frame_count = model.frame_count + 1 }, tick));
    view =
      (fun model ->
        let title = Style.render Style.(empty |> bold true) "Space" in
        let rows =
          List.init
            (max 0 (model.height - 1))
            (fun y -> String.concat "" (List.init model.width (fun x -> cell model y x)))
        in
        Charamel_tea.View.v ~alt_screen:true (String.concat "\n" (title :: rows)));
    subscriptions =
      (fun _ ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.resize (fun ~rows ~cols -> Resize (rows, cols));
          ]);
  }

let main () = Smoke.run_ app
let blocks n = String.concat "" (List.init n (fun _ -> "▀"))

let smoke () =
  Smoke.expect app [ Smoke.key "q" ] [ "Space"; "\n" ^ blocks 80 ^ "\n" ]
  @ Smoke.expect app [ `Resize (24, 40); Smoke.key "q" ] [ "\n" ^ blocks 40 ^ "\n" ]
  @ Smoke.expect app [ `Wait 0.5; Smoke.key "q" ] [ "Space" ]
