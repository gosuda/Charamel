module Cmd = Charm_tea.Cmd
module Sub = Charm_tea.Sub
module Style = Charm_lipgloss.Style
module Color = Charm_ansi.Color
module Text = Charm_ansi.Text
module Spring = Charm_harmonica.Spring

type msg = Frame

type t = {
  width : int;
  colors : Color.t list option;
  scaled : bool;
  color_func : (total:float -> current:float -> Color.t) option;
  full : string;
  empty : string;
  full_color : Color.t;
  empty_color : Color.t;
  show_percentage : bool;
  percent_format : float -> string;
  percentage_style : Style.t;
  spring : Spring.t;
  shown : float;
  target : float;
  velocity : float;
}

let frame_interval = 1. /. 60.
let default_full_color = Color.Rgb (117, 113, 249)
let default_empty_color = Color.Rgb (96, 96, 96)
let default_blend_start = Color.Rgb (90, 86, 224)
let default_blend_end = Color.Rgb (238, 111, 248)
let clamp p = Float.max 0.0 (Float.min 1.0 p)

let make_spring ~frequency ~damping =
  Spring.v ~delta_time:frame_interval ~angular_frequency:frequency ~damping_ratio:damping

let default_percent_format percent = Fmt.str " %3.0f%%" percent

let v ?(width = 40) ?colors ?(scaled = false) ?color_func ?(full = "█") ?(empty = "░")
    ?(show_percentage = true) ?(percent_format = default_percent_format)
    ?(percentage_style = Style.empty) ?(spring = (18.0, 1.0)) () =
  let colors =
    Some (Option.value colors ~default:[ default_blend_start; default_blend_end ])
  in
  let full_color =
    match colors with Some [ color ] -> color | _ -> default_full_color
  in
  let spring_frequency, spring_damping = spring in
  {
    width = max 0 width;
    colors;
    scaled;
    color_func;
    full;
    empty;
    full_color;
    empty_color = default_empty_color;
    show_percentage;
    percent_format;
    percentage_style;
    spring = make_spring ~frequency:spring_frequency ~damping:spring_damping;
    shown = 0.0;
    target = 0.0;
    velocity = 0.0;
  }

let is_animating t =
  let distance = Float.abs (t.shown -. t.target) in
  not (distance < 0.001 && Float.abs t.velocity < 0.01)

let update Frame t =
  if not (is_animating t) then (t, Cmd.none)
  else
    let shown, velocity =
      Spring.update t.spring ~pos:t.shown ~vel:t.velocity ~target:t.target
    in
    ({ t with shown; velocity }, Cmd.none)

let rgb_or_default color =
  match Color.to_rgb color with Some rgb -> rgb | None -> (0, 0, 0)

let interpolate a b f =
  let ar, ag, ab = rgb_or_default a in
  let br, bg, bb = rgb_or_default b in
  let component x y = int_of_float (Float.round (float x +. (float (y - x) *. f))) in
  match Color.rgb (component ar br) (component ag bg) (component ab bb) with
  | Some color -> color
  | None -> Color.Default

let blend_values ~count stops =
  if count <= 0 || stops = [] then []
  else if Stdlib.List.length stops = 1 then
    Stdlib.List.init count (fun _ -> Stdlib.List.hd stops)
  else if count = 1 then [ Stdlib.List.hd stops ]
  else
    let stop_count = Stdlib.List.length stops in
    let stop_at i = Stdlib.List.nth stops i in
    Stdlib.List.init count (fun i ->
        let position = float i /. float (count - 1) in
        let scaled = position *. float (stop_count - 1) in
        let lower = int_of_float (Float.floor scaled) in
        let upper = min (stop_count - 1) (lower + 1) in
        let fraction = scaled -. float lower in
        interpolate (stop_at lower) (stop_at upper) fraction)

let repeat value count =
  if count <= 0 || value = "" then ""
  else
    let out = Buffer.create (String.length value * count) in
    for _ = 1 to count do
      Buffer.add_string out value
    done;
    Buffer.contents out

let styled_fill style value = if value = "" then "" else Style.render style value

let percentage_view t percent =
  if not t.show_percentage then ""
  else
    let percent = clamp percent *. 100. in
    Style.render (Style.inline true t.percentage_style) (t.percent_format percent)

let bar_view t percent text_width =
  let total_width = max 0 (t.width - text_width) in
  let filled_width =
    max 0
      (min total_width (int_of_float (Float.round (float total_width *. clamp percent))))
  in
  let half_block = t.full = "▌" in
  let full_style color = Style.foreground color Style.empty in
  let empty_style = full_style t.empty_color in
  let filled =
    match t.color_func with
    | Some color_func ->
        let out = Buffer.create (String.length t.full * filled_width) in
        let denominator = if total_width <= 0 then 1. else float total_width in
        for i = 0 to filled_width - 1 do
          let current = float i /. denominator in
          let foreground = color_func ~total:percent ~current in
          let style =
            if half_block then
              let half = 0.5 /. denominator in
              let background =
                color_func ~total:percent ~current:(min 1. (current +. half))
              in
              Style.background background (full_style foreground)
            else full_style foreground
          in
          Buffer.add_string out (Style.render style t.full)
        done;
        Buffer.contents out
    | None -> (
        match t.colors with
        | Some (_ :: _ :: _ as stops) ->
            let sample_width =
              if t.scaled then filled_width * if half_block then 2 else 1
              else total_width * if half_block then 2 else 1
            in
            let values = blend_values ~count:sample_width stops in
            let out = Buffer.create (String.length t.full * filled_width) in
            for i = 0 to filled_width - 1 do
              if half_block then begin
                let offset = i * 2 in
                let foreground =
                  match Stdlib.List.nth_opt values offset with
                  | Some c -> c
                  | None -> default_full_color
                in
                let background =
                  match Stdlib.List.nth_opt values (offset + 1) with
                  | Some c -> c
                  | None -> foreground
                in
                let style = Style.background background (full_style foreground) in
                Buffer.add_string out (Style.render style t.full)
              end
              else
                let color =
                  match Stdlib.List.nth_opt values i with
                  | Some c -> c
                  | None -> default_full_color
                in
                Buffer.add_string out (Style.render (full_style color) t.full)
            done;
            Buffer.contents out
        | _ -> styled_fill (full_style t.full_color) (repeat t.full filled_width))
  in
  let empty = styled_fill empty_style (repeat t.empty (total_width - filled_width)) in
  filled ^ empty

let view_as percent t =
  let percent = clamp percent in
  let percentage = percentage_view t percent in
  bar_view t percent (Text.width percentage) ^ percentage

let view t = view_as t.shown t
let key _ _ = None

let subscriptions t =
  if is_animating t then Sub.every frame_interval (fun _ -> Frame) else Sub.none

let percent t = t.target
let set_percent percent t = { t with target = clamp percent }
let incr_percent amount t = set_percent (t.target +. amount) t
let decr_percent amount t = set_percent (t.target -. amount) t
let width t = t.width
let set_width width t = { t with width = max 0 width }

let set_spring_options ~frequency ~damping t =
  { t with spring = make_spring ~frequency ~damping }

let set_show_percentage show_percentage t = { t with show_percentage }
