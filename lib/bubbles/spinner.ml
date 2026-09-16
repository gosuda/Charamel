module Cmd = Charm_tea.Cmd
module Sub = Charm_tea.Sub

(* The frame literals are transcribed from .references/bubbles/spinner/spinner.go:26-84,
   charmbracelet/bubbles, MIT. *)
module Style = Charm_lipgloss.Style

type kind =
  | Line
  | Dot
  | Mini_dot
  | Jump
  | Pulse
  | Points
  | Globe
  | Moon
  | Monkey
  | Meter
  | Hamburger
  | Ellipsis

let line_frames = [ "|"; "/"; "-"; "\\" ]
let dot_frames = [ "⣾ "; "⣽ "; "⣻ "; "⢿ "; "⡿ "; "⣟ "; "⣯ "; "⣷ " ]

let mini_dot_frames = [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ]

let jump_frames = [ "⢄"; "⢂"; "⢁"; "⡁"; "⡈"; "⡐"; "⡠" ]
let pulse_frames = [ "█"; "▓"; "▒"; "░" ]
let points_frames = [ "∙∙∙"; "●∙∙"; "∙●∙"; "∙∙●" ]
let globe_frames = [ "🌍"; "🌎"; "🌏" ]
let moon_frames = [ "🌑"; "🌒"; "🌓"; "🌔"; "🌕"; "🌖"; "🌗"; "🌘" ]
let monkey_frames = [ "🙈"; "🙉"; "🙊" ]

let meter_frames = [ "▱▱▱"; "▰▱▱"; "▰▰▱"; "▰▰▰"; "▰▰▱"; "▰▱▱"; "▱▱▱" ]

let hamburger_frames = [ "☱"; "☲"; "☴"; "☲" ]
let ellipsis_frames = [ ""; "."; ".."; "..." ]

let frames = function
  | Line -> line_frames
  | Dot -> dot_frames
  | Mini_dot -> mini_dot_frames
  | Jump -> jump_frames
  | Pulse -> pulse_frames
  | Points -> points_frames
  | Globe -> globe_frames
  | Moon -> moon_frames
  | Monkey -> monkey_frames
  | Meter -> meter_frames
  | Hamburger -> hamburger_frames
  | Ellipsis -> ellipsis_frames

let fps = function
  | Line | Dot | Jump -> 1. /. 10.
  | Mini_dot -> 1. /. 12.
  | Pulse | Moon -> 1. /. 8.
  | Points | Meter -> 1. /. 7.
  | Globe -> 1. /. 4.
  | Hamburger | Ellipsis | Monkey -> 1. /. 3.

let kind_of_string s =
  match String.lowercase_ascii s with
  | "line" -> Some Line
  | "dot" -> Some Dot
  | "minidot" | "mini-dot" | "mini_dot" -> Some Mini_dot
  | "jump" -> Some Jump
  | "pulse" -> Some Pulse
  | "points" -> Some Points
  | "globe" -> Some Globe
  | "moon" -> Some Moon
  | "monkey" -> Some Monkey
  | "meter" -> Some Meter
  | "hamburger" -> Some Hamburger
  | "ellipsis" -> Some Ellipsis
  | _ -> None

type msg = Tick

type t = {
  kind : kind option;
  frames : string list;
  frame_interval : float;
  frame : int;
  style : Style.t;
}

let v ?(kind = Line) ?frames:custom_frames ?(style = Style.empty) () =
  match custom_frames with
  | Some (values, interval) ->
      let values = if values = [] then [ "" ] else values in
      {
        kind = None;
        frames = values;
        frame_interval = max 0.000001 interval;
        frame = 0;
        style;
      }
  | None ->
      {
        kind = Some kind;
        frames = frames kind;
        frame_interval = fps kind;
        frame = 0;
        style;
      }

let update Tick t =
  let frame =
    if t.frames = [] then 0 else (t.frame + 1) mod Stdlib.List.length t.frames
  in
  ({ t with frame }, Cmd.none)

let view t =
  let frame =
    match Stdlib.List.nth_opt t.frames t.frame with Some frame -> frame | None -> ""
  in
  Style.render t.style frame

let key _ _ = None
let subscriptions t = Sub.every t.frame_interval (fun _ -> Tick)

let set_kind kind t =
  { t with kind = Some kind; frames = frames kind; frame_interval = fps kind; frame = 0 }

let set_style style t = { t with style }
let kind t = t.kind
