module Color = Charamel_ansi.Color
module Style = Charamel_lipgloss.Style

let progress_bar_width = 71
let progress_full_char = "█"
let progress_empty_char = "░"
let dot_char = " • "
let keyword_style = Style.foreground (Color.Indexed 211) Style.empty
let subtle_style = Style.foreground (Color.Indexed 241) Style.empty
let ticks_style = Style.foreground (Color.Indexed 79) Style.empty
let checkbox_style = Style.foreground (Color.Indexed 212) Style.empty
let dot_style = Style.foreground (Color.Indexed 236) Style.empty
let main_style = Style.margin_side `Left 2 Style.empty
let progress_empty = Style.render subtle_style progress_empty_char
let dot = Style.render dot_style dot_char

let ramp =
  List.map
    (fun color -> Style.foreground color Style.empty)
    (Charamel_lipgloss.Blending.blend1d ~steps:progress_bar_width
       [ Color.of_hex_or "#B14FFF"; Color.of_hex_or "#00FFA3" ])

let repeat text count = String.concat "" (List.init count (fun _ -> text))

let out_bounce x =
  let n1 = 7.5625 and d1 = 2.75 in
  let below a b = Float.compare a b < 0 in
  if below x (1. /. d1) then n1 *. x *. x
  else if below x (2. /. d1) then
    let a = x -. (1.5 /. d1) in
    (n1 *. a *. a) +. 0.75
  else if below x (2.5 /. d1) then
    let a = x -. (2.25 /. d1) in
    (n1 *. a *. a) +. 0.9375
  else
    let a = x -. (3. /. d1) in
    (n1 *. a *. a) +. 0.984375

let progressbar percent =
  let full_size =
    int_of_float (Float.round (float_of_int progress_bar_width *. percent))
  in
  let full_cells =
    String.concat ""
      (List.init full_size (fun i -> Style.render (List.nth ramp i) progress_full_char))
  in
  let empty_cells = repeat progress_empty (progress_bar_width - full_size) in
  full_cells ^ empty_cells ^ Fmt.str " %3.0f" (Float.round (percent *. 100.))

let checkbox label selected =
  if selected then Style.render checkbox_style ("[x] " ^ label)
  else Fmt.str "[ ] %s" label

type model = {
  choice : int;
  chosen : bool;
  ticks : int;
  frames : int;
  progress : float;
  loaded : bool;
  quitting : bool;
}

type msg = Key of Charamel_tea.Key.t | Tick | Frame

let none = Charamel_tea.Cmd.none

let update_choices msg model =
  match msg with
  | Key key -> (
      match Charamel_tea.Key.to_string key with
      | "j" | "down" ->
          ({ model with choice = min 3 (model.choice + 1) }, Charamel_tea.Cmd.none)
      | "k" | "up" ->
          ({ model with choice = max 0 (model.choice - 1) }, Charamel_tea.Cmd.none)
      | "enter" -> ({ model with chosen = true }, none)
      | _ -> (model, Charamel_tea.Cmd.none))
  | Tick ->
      if Int.equal model.ticks 0 then
        ({ model with quitting = true }, Charamel_tea.Cmd.quit)
      else ({ model with ticks = model.ticks - 1 }, none)
  | Frame -> (model, Charamel_tea.Cmd.none)

let update_chosen msg model =
  match msg with
  | Frame ->
      if model.loaded then (model, Charamel_tea.Cmd.none)
      else
        let frames = model.frames + 1 in
        let progress = out_bounce (float_of_int frames /. 100.) in
        if Float.compare progress 1. >= 0 then
          ({ model with frames; progress = 1.; loaded = true; ticks = 3 }, none)
        else ({ model with frames; progress }, none)
  | Tick ->
      if not model.loaded then (model, Charamel_tea.Cmd.none)
      else if Int.equal model.ticks 0 then
        ({ model with quitting = true }, Charamel_tea.Cmd.quit)
      else ({ model with ticks = model.ticks - 1 }, none)
  | Key _ -> (model, Charamel_tea.Cmd.none)

let is_quit name =
  String.equal name "q" || String.equal name "escape" || String.equal name "ctrl+c"

let update msg model =
  match msg with
  | Key key ->
      let name = Charamel_tea.Key.to_string key in
      if is_quit name then ({ model with quitting = true }, Charamel_tea.Cmd.quit)
      else if model.chosen then update_chosen msg model
      else update_choices msg model
  | Tick | Frame ->
      if model.chosen then update_chosen msg model else update_choices msg model

let choices_view model =
  let choices =
    String.concat "\n"
      [
        checkbox "Plant carrots" (Int.equal model.choice 0);
        checkbox "Go to the market" (Int.equal model.choice 1);
        checkbox "Read something" (Int.equal model.choice 2);
        checkbox "See friends" (Int.equal model.choice 3);
      ]
  in
  "What to do today?\n\n" ^ choices ^ "\n\nProgram quits in "
  ^ Style.render ticks_style (string_of_int model.ticks)
  ^ " seconds\n\n"
  ^ Style.render subtle_style "j/k, up/down: select"
  ^ dot
  ^ Style.render subtle_style "enter: choose"
  ^ dot
  ^ Style.render subtle_style "q, esc: quit"

let chosen_view model =
  let keyword text = Style.render keyword_style text in
  let msg =
    match model.choice with
    | 0 ->
        Fmt.str "Carrot planting?\n\nCool, we'll need %s and %s..." (keyword "libgarden")
          (keyword "vegeutils")
    | 1 ->
        Fmt.str "A trip to the market?\n\nOkay, then we should install %s and %s..."
          (keyword "marketkit") (keyword "libshopping")
    | 2 ->
        Fmt.str "Reading time?\n\nOkay, cool, then we’ll need a library. Yes, an %s."
          (keyword "actual library")
    | _ ->
        Fmt.str "It’s always good to see friends.\n\nFetching %s and %s..."
          (keyword "social-skills") (keyword "conversationutils")
  in
  let label =
    if model.loaded then
      "Downloaded. Exiting in "
      ^ Style.render ticks_style (string_of_int model.ticks)
      ^ " seconds..."
    else "Downloading..."
  in
  msg ^ "\n\n" ^ label ^ "\n" ^ progressbar model.progress ^ "%"

let view model =
  let content =
    if model.quitting then "\n  See you later!\n\n"
    else
      let s = if model.chosen then chosen_view model else choices_view model in
      Style.render main_style ("\n" ^ s ^ "\n")
  in
  Charamel_tea.View.v content

let initial =
  {
    choice = 0;
    chosen = false;
    ticks = 10;
    frames = 0;
    progress = 0.;
    loaded = false;
    quitting = false;
  }

let tick_active model = (not model.chosen) || model.loaded

let subscriptions model =
  let countdown =
    if tick_active model then Charamel_tea.Sub.every 1. (fun _ -> Tick)
    else Charamel_tea.Sub.none
  in
  let frames =
    if model.chosen && not model.loaded then
      Charamel_tea.Sub.every (1. /. 60.) (fun _ -> Frame)
    else Charamel_tea.Sub.none
  in
  Charamel_tea.Sub.batch [ Charamel_tea.Sub.key (fun key -> Key key); countdown; frames ]

let app : (model, msg) Charamel_tea.app =
  { init = (fun () -> (initial, none)); update; view; subscriptions }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app []
    [ "What to do today?"; "[x] Plant carrots"; "Program quits in 10 seconds" ]
  @ Smoke.expect app [ Smoke.key "down" ] [ "[x] Go to the market"; "[ ] Plant carrots" ]
  @ Smoke.expect app
      [ Smoke.key "j"; Smoke.key "j"; Smoke.key "j"; Smoke.key "j" ]
      [ "[x] See friends" ]
  @ Smoke.expect app
      [ Smoke.key "enter" ]
      [ "Carrot planting?"; "Downloading..."; "libgarden" ]
  @ Smoke.expect app [ Smoke.key "down"; Smoke.key "enter" ] [ "A trip to the market?" ]
  @ Smoke.expect app
      [ Smoke.key "enter"; `Wait 1.7 ]
      [ "Downloaded. Exiting in 3 seconds..." ]
  @ Smoke.expect app [ `Wait 10.5 ] [ "Program quits in 0 seconds" ]
