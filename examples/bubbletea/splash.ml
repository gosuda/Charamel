module Color = Charamel_ansi.Color
module Style = Charamel_lipgloss.Style

let colors =
  List.map Color.of_hex_or
    [
      "#881177";
      "#aa3355";
      "#cc6666";
      "#ee9944";
      "#eedd00";
      "#99dd55";
      "#44dd88";
      "#22ccbb";
      "#00bbcc";
      "#0099cc";
      "#3366bb";
      "#663399";
    ]

let color_count = List.length colors
let rate = 90.
let tick_interval = 1. /. 60.
let components color = Option.value ~default:(0, 0, 0) (Color.to_rgb color)

let interpolate first second t =
  let r1, g1, b1 = components first in
  let r2, g2, b2 = components second in
  let mix one two =
    int_of_float ((float_of_int one *. (1. -. t)) +. (float_of_int two *. t))
  in
  Color.Rgb (mix r1 r2, mix g1 g2, mix b1 b2)

let gradient_color position =
  let clamped = Float.max 0. (Float.min 1. position) in
  let index = clamped *. float_of_int (color_count - 1) in
  let first = int_of_float (Float.floor index) in
  let second = int_of_float (Float.ceil index) in
  interpolate
    (List.nth colors (first mod color_count))
    (List.nth colors (second mod color_count))
    (index -. float_of_int first)

let half_block = "▀"
let repeat count glyph = String.concat "" (List.init count (fun _ -> glyph))

let cell foreground background =
  Style.render
    (Style.background background (Style.foreground foreground Style.empty))
    half_block

let row cells width center_x center_y cos_angle sin_angle line_y =
  let point_y = (float_of_int line_y *. 2.) -. center_y in
  let left_x = -.center_x in
  let start_x =
    (center_x +. ((left_x *. cos_angle) -. (point_y *. sin_angle))) /. width
  in
  let start_next_x =
    (center_x +. ((left_x *. cos_angle) -. ((point_y +. 1.) *. sin_angle))) /. width
  in
  let end_x =
    (center_x +. ((center_x *. cos_angle) -. (point_y *. sin_angle))) /. width
  in
  let delta_x = (end_x -. start_x) /. width in
  if Float.compare (Float.abs delta_x) 0.0001 < 0 then
    repeat cells (cell (gradient_color start_x) (gradient_color start_next_x))
  else
    String.concat ""
      (List.init cells (fun column ->
           let offset = float_of_int column *. delta_x in
           cell
             (gradient_color (start_x +. offset))
             (gradient_color (start_next_x +. offset))))

let gradient width height time =
  let w = float_of_int width and h = float_of_int height in
  let angle = -.time *. rate *. Float.pi /. 180. in
  let cos_angle = Float.cos angle and sin_angle = Float.sin angle in
  String.concat "\n" (List.init height (row width w (w /. 2.) h cos_angle sin_angle))

type model = { width : int; height : int; time : float }
type msg = Key of Charamel_tea.Key.t | Win of { rows : int; cols : int } | Tick

let tick = Charamel_tea.Cmd.after tick_interval (fun () -> Tick)

let update msg model =
  match msg with
  | Key _ -> (model, Charamel_tea.Cmd.quit)
  | Win { rows; cols } ->
      ({ model with width = cols; height = rows }, Charamel_tea.Cmd.none)
  | Tick -> ({ model with time = model.time +. tick_interval }, tick)

let view model =
  let content =
    if Int.equal model.width 0 then "Initializing..."
    else gradient model.width model.height model.time
  in
  Charamel_tea.View.v ~alt_screen:true content

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ width = 0; height = 0; time = 0. }, tick));
    update;
    view;
    subscriptions =
      (fun _ ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.resize (fun ~rows ~cols -> Win { rows; cols });
          ]);
  }

let main () = Smoke.run_ app
let row_of width = repeat width half_block

let smoke () =
  Smoke.expect app [ Smoke.key "q" ] [ "\n" ^ row_of 80 ^ "\n" ]
  @ Smoke.expect app ~size:(6, 33) [ Smoke.key "q" ] [ "\n" ^ row_of 33 ^ "\n" ]
  @ Smoke.expect app [ `Resize (12, 60); Smoke.key "q" ] [ "\n" ^ row_of 60 ^ "\n" ]
