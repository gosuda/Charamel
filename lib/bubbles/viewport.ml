module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module Style = Charamel_lipgloss.Style
module Text = Charamel_ansi.Text
module Layout = Charamel_lipgloss.Layout

let clamp n lo hi =
  let lo, hi = if lo <= hi then (lo, hi) else (hi, lo) in
  max lo (min hi n)

let normalize_crlf s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then ()
    else if
      i + 1 < String.length s && String.get s i = '\r' && String.get s (i + 1) = '\n'
    then (
      Buffer.add_char b '\n';
      loop (i + 2))
    else (
      Buffer.add_char b (String.get s i);
      loop (i + 1))
  in
  loop 0;
  Buffer.contents b

let frame_sides style =
  let padding =
    match Style.get_padding style with
    | Some s -> s
    | None -> Charamel_lipgloss.Sides.v ()
  in
  let border = Style.get_border style in
  let side_size getter enabled =
    match (border, enabled) with Some b, Some true -> getter b | _ -> 0
  in
  let left =
    padding.Charamel_lipgloss.Sides.left
    + side_size Charamel_lipgloss.Border.left_size (Style.get_border_left style)
  in
  let right =
    padding.Charamel_lipgloss.Sides.right
    + side_size Charamel_lipgloss.Border.right_size (Style.get_border_right style)
  in
  let top =
    padding.Charamel_lipgloss.Sides.top
    + side_size Charamel_lipgloss.Border.top_size (Style.get_border_top style)
  in
  let bottom =
    padding.Charamel_lipgloss.Sides.bottom
    + side_size Charamel_lipgloss.Border.bottom_size (Style.get_border_bottom style)
  in
  (left, right, top, bottom)

let frame_width style =
  let left, right, _, _ = frame_sides style in
  left + right

let frame_height style =
  let _, _, top, bottom = frame_sides style in
  top + bottom

type keymap = {
  page_down : Key_binding.t;
  page_up : Key_binding.t;
  half_page_up : Key_binding.t;
  half_page_down : Key_binding.t;
  up : Key_binding.t;
  down : Key_binding.t;
  left : Key_binding.t;
  right : Key_binding.t;
}

let default_keymap =
  {
    page_down = Key_binding.v ~help:("f/pgdn", "page down") [ "pgdown"; "space"; "f" ];
    page_up = Key_binding.v ~help:("b/pgup", "page up") [ "pgup"; "b" ];
    half_page_up = Key_binding.v ~help:("u", "½ page up") [ "u"; "ctrl+u" ];
    half_page_down = Key_binding.v ~help:("d", "½ page down") [ "d"; "ctrl+d" ];
    up = Key_binding.v ~help:("↑/k", "up") [ "up"; "k" ];
    down = Key_binding.v ~help:("↓/j", "down") [ "down"; "j" ];
    left = Key_binding.v ~help:("←/h", "move left") [ "left"; "h" ];
    right = Key_binding.v ~help:("→/l", "move right") [ "right"; "l" ];
  }

type gutter_context = { index : int; total_lines : int; soft : bool }

type msg =
  | Page_down
  | Page_up
  | Half_page_up
  | Half_page_down
  | Up
  | Down
  | Left
  | Right
  | Wheel of Charamel_tea.Mouse.t

type highlight = { line_start : int; ranges : (int * int * int) list }

type t = {
  width : int;
  height : int;
  keymap : keymap;
  style : Style.t;
  soft_wrap : bool;
  fill_height : bool;
  mouse_wheel_enabled : bool;
  mouse_wheel_delta : int;
  horizontal_step : int;
  left_gutter : (gutter_context -> string) option;
  style_line : (int -> Style.t) option;
  highlight_style : Style.t;
  selected_highlight_style : Style.t;
  lines : string list;
  longest_line_width : int;
  y_offset : int;
  x_offset : int;
  highlights : highlight list;
  highlight_index : int;
}

let max_line_width lines =
  Stdlib.List.fold_left (fun n line -> max n (Text.width line)) 0 lines

let line_split s =
  let s = normalize_crlf s in
  String.split_on_char '\n' s

let flatten_lines lines =
  let add acc line =
    let parts = line_split line in
    Stdlib.List.rev_append (Stdlib.List.rev parts) acc
  in
  let out = Stdlib.List.fold_left add [] lines |> Stdlib.List.rev in
  match out with [] -> [ "" ] | [ "" ] -> [ "" ] | xs -> xs

let v ?(width = 0) ?(height = 0) ?(keymap = default_keymap) ?(style = Style.empty)
    ?(soft_wrap = false) ?(fill_height = false) ?(mouse_wheel_enabled = true)
    ?(mouse_wheel_delta = 3) ?(horizontal_step = 6) ?left_gutter ?style_line
    ?(highlight_style = Style.empty) ?(selected_highlight_style = Style.empty) () =
  {
    width = max 0 width;
    height = max 0 height;
    keymap;
    style;
    soft_wrap;
    fill_height;
    mouse_wheel_enabled;
    mouse_wheel_delta = max 0 mouse_wheel_delta;
    horizontal_step = max 0 horizontal_step;
    left_gutter;
    style_line;
    highlight_style;
    selected_highlight_style;
    lines = [ "" ];
    longest_line_width = 0;
    y_offset = 0;
    x_offset = 0;
    highlights = [];
    highlight_index = -1;
  }

let effective_width m =
  match Style.get_width m.style with Some n when n > 0 -> min m.width n | _ -> m.width

let effective_height m =
  match Style.get_height m.style with
  | Some n when n > 0 -> min m.height n
  | _ -> m.height

let gutter_width m =
  match m.left_gutter with
  | None -> 0
  | Some f ->
      Text.width (f { index = 0; total_lines = Stdlib.List.length m.lines; soft = false })

let max_width m = max 0 (effective_width m - frame_width m.style - gutter_width m)
let max_height m = max 0 (effective_height m - frame_height m.style)

let wrapped_line_count m =
  if not m.soft_wrap then Stdlib.List.length m.lines
  else
    let w = max_width m in
    if w <= 0 then 0
    else
      Stdlib.List.fold_left
        (fun n line -> n + max 1 ((Text.width line + w - 1) / w))
        0 m.lines

let max_y_offset m = max 0 (wrapped_line_count m - max_height m)

let max_x_offset m =
  let content_width = max 0 (effective_width m - frame_width m.style) in
  max 0 (m.longest_line_width - content_width)

let apply_style_line m index line =
  match m.style_line with None -> line | Some f -> Style.render (f index) line

let apply_highlights m index line =
  let ranges =
    let rec loop i acc = function
      | [] -> Stdlib.List.rev acc
      | h :: rest ->
          let acc =
            let selected = i = m.highlight_index in
            let style =
              if selected then m.selected_highlight_style else m.highlight_style
            in
            let current =
              Stdlib.List.filter_map
                (fun (line, a, b) -> if line = index then Some (a, b, style) else None)
                h.ranges
            in
            Stdlib.List.rev_append current acc
          in
          loop (i + 1) acc rest
    in
    loop 0 [] m.highlights
  in
  if ranges = [] then line else Layout.style_ranges ranges line

let gutter m ~index ~total_lines ~soft line =
  match m.left_gutter with
  | None -> line
  | Some f -> f { index; total_lines; soft } ^ line

let chunk_line line ~left ~right = Text.cut ~left ~right line

let concat_mapi f xs =
  let rec loop i acc = function
    | [] -> Stdlib.List.rev acc
    | x :: rest -> loop (i + 1) (Stdlib.List.rev_append (f i x) acc) rest
  in
  loop 0 [] xs

let build_visible m =
  let mh = max_height m in
  let mw = max_width m in
  if m.lines = [ "" ] && not m.fill_height then []
  else if mh <= 0 || mw <= 0 then []
  else if m.soft_wrap then
    let total = wrapped_line_count m in
    let chunks =
      concat_mapi
        (fun idx line ->
          let line = line |> apply_style_line m idx |> apply_highlights m idx in
          let w = Text.width line in
          if w <= mw then [ gutter m ~index:idx ~total_lines:total ~soft:false line ]
          else
            let count = max 1 ((w + mw - 1) / mw) in
            Stdlib.List.init count (fun part ->
                let chunk = chunk_line line ~left:(part * mw) ~right:((part + 1) * mw) in
                gutter m ~index:idx ~total_lines:total ~soft:(part > 0) chunk))
        m.lines
    in
    let start = clamp m.y_offset 0 (max 0 (Stdlib.List.length chunks)) in
    let visible =
      if start >= Stdlib.List.length chunks then []
      else Stdlib.List.filteri (fun i _ -> i >= start && i < start + mh) chunks
    in
    if m.fill_height && Stdlib.List.length visible < mh then
      let missing = mh - Stdlib.List.length visible in
      let first = start + Stdlib.List.length visible in
      visible
      @ Stdlib.List.init missing (fun i ->
          gutter m ~index:(first + i) ~total_lines:total ~soft:false "")
    else visible
  else
    let total = Stdlib.List.length m.lines in
    let start = clamp m.y_offset 0 total in
    let selected =
      Stdlib.List.filteri (fun i _ -> i >= start && i < start + mh) m.lines
    in
    let base =
      Stdlib.List.mapi
        (fun i line ->
          let index = i + start in
          let line = line |> apply_style_line m index |> apply_highlights m index in
          let line = chunk_line line ~left:m.x_offset ~right:(m.x_offset + mw) in
          gutter m ~index ~total_lines:total ~soft:false line)
        selected
    in
    if m.fill_height && Stdlib.List.length base < mh then
      let missing = mh - Stdlib.List.length base in
      let first = start + Stdlib.List.length base in
      base
      @ Stdlib.List.init missing (fun i ->
          gutter m ~index:(first + i) ~total_lines:total ~soft:false "")
    else base

let visible_lines m = build_visible m

let view m =
  let w = effective_width m and h = effective_height m in
  if w <= 0 || h <= 0 then ""
  else
    let fw = frame_width m.style and fh = frame_height m.style in
    let inner_w = max 0 (w - fw) and inner_h = max 0 (h - fh) in
    if inner_w <= 0 || inner_h <= 0 then
      Style.render (Style.unset_width (Style.unset_height m.style)) ""
    else
      let content = String.concat "\n" (visible_lines m) in
      let content =
        Style.render (Style.width inner_w (Style.height inner_h Style.empty)) content
      in
      Style.render (Style.unset_width (Style.unset_height m.style)) content

let content m = String.concat "\n" m.lines
let width m = m.width
let height m = m.height
let y_offset m = m.y_offset
let x_offset m = m.x_offset
let style m = m.style
let soft_wrap m = m.soft_wrap
let total_line_count m = wrapped_line_count m
let visible_line_count m = Stdlib.List.length (visible_lines m)
let at_top m = m.y_offset <= 0
let at_bottom m = m.y_offset >= max_y_offset m
let past_bottom m = m.y_offset > max_y_offset m

let scroll_percent m =
  let total = wrapped_line_count m and h = max_height m in
  if h >= total || total <= h then 1.0
  else max 0. (min 1. (float m.y_offset /. float (total - h)))

let horizontal_scroll_percent m =
  let total = m.longest_line_width and w = max_width m in
  if w >= total || total <= w then 1.0
  else max 0. (min 1. (float m.x_offset /. float (total - w)))

let set_y_offset n m = { m with y_offset = clamp n 0 (max_y_offset m) }

let set_x_offset n m =
  if m.soft_wrap then m else { m with x_offset = clamp n 0 (max_x_offset m) }

let set_width n m =
  { m with width = max 0 n } |> fun m ->
  set_y_offset m.y_offset (set_x_offset m.x_offset m)

let set_height n m = { m with height = max 0 n } |> fun m -> set_y_offset m.y_offset m

let set_content_lines lines m =
  let lines = flatten_lines lines in
  let longest_line_width = max_line_width lines in
  let m = { m with lines; longest_line_width; highlights = []; highlight_index = -1 } in
  if m.y_offset > max_y_offset m then set_y_offset (max_y_offset m) m else m

let set_content s m = set_content_lines (line_split s) m

let set_style style m =
  { m with style } |> fun m -> set_y_offset m.y_offset (set_x_offset m.x_offset m)

let set_soft_wrap soft_wrap m =
  { m with soft_wrap; x_offset = (if soft_wrap then 0 else m.x_offset) } |> fun m ->
  set_y_offset m.y_offset m

let set_fill_height fill_height m = { m with fill_height }

let set_left_gutter left_gutter m =
  { m with left_gutter } |> fun m -> set_y_offset m.y_offset m

let set_style_line style_line m = { m with style_line }
let set_horizontal_step n m = { m with horizontal_step = max 0 n }
let set_mouse_wheel_enabled enabled m = { m with mouse_wheel_enabled = enabled }
let set_mouse_wheel_delta n m = { m with mouse_wheel_delta = max 0 n }
let scroll_down n m = if n = 0 then m else set_y_offset (m.y_offset + n) m
let scroll_up n m = if n = 0 then m else set_y_offset (m.y_offset - n) m
let scroll_left n m = set_x_offset (m.x_offset - n) m
let scroll_right n m = set_x_offset (m.x_offset + n) m
let goto_top m = set_y_offset 0 m
let goto_bottom m = set_y_offset (max_y_offset m) m
let page_down m = scroll_down (max_height m) m
let page_up m = scroll_up (max_height m) m
let half_page_down m = scroll_down (max_height m / 2) m
let half_page_up m = scroll_up (max_height m / 2) m

let ensure_visible ~line ~colstart ~colend m =
  let mw = max_width m in
  let m =
    if colend <= mw then set_x_offset 0 m
    else set_x_offset (colstart - m.horizontal_step) m
  in
  if not m.soft_wrap then
    if line < m.y_offset then set_y_offset line m
    else if line >= m.y_offset + max_height m then set_y_offset line m
    else m
  else
    let rec find_y y i = function
      | [] -> y
      | line' :: rest ->
          if i = line then y
          else
            find_y
              (y
              + max 1 ((Text.width (line' |> apply_style_line m i) + mw - 1) / max 1 mw))
              (i + 1) rest
    in
    let y = find_y 0 0 m.lines in
    if y < m.y_offset || y >= m.y_offset + max_height m then set_y_offset y m else m

let parse_highlights content matches =
  let lines = line_split content in
  let starts =
    let rec loop offset acc = function
      | [] -> Stdlib.List.rev acc
      | line :: rest ->
          let e = offset + String.length line in
          loop (e + 1) ((offset, e) :: acc) rest
    in
    loop 0 [] lines
  in
  let line_for_byte byte =
    let rec loop i = function
      | [] -> (0, 0, 0)
      | (a, b) :: rest -> if byte <= b then (i, a, b) else loop (i + 1) rest
    in
    loop 0 starts
  in
  let one (a, b) =
    let a, b = (min a b, max a b) in
    let first, _, _ = line_for_byte a in
    let last, _, _ = line_for_byte (max a (b - 1)) in
    let ranges =
      Stdlib.List.fold_left
        (fun acc (idx, (ls, le)) ->
          if idx < first || idx > last then acc
          else
            let local_a = max a ls - ls and local_b = min b le - ls in
            let crosses_newline = b > le in
            if local_b <= local_a && not crosses_newline then acc
            else
              let line = Stdlib.List.nth lines idx in
              let clipped_a = min (String.length line) (max 0 local_a) in
              let clipped_b = min (String.length line) (max 0 local_b) in
              let sa = Text.width (String.sub line 0 clipped_a) in
              let sb =
                Text.width (String.sub line 0 clipped_b)
                + if crosses_newline then 1 else 0
              in
              if sb <= sa then acc else (idx, sa, sb) :: acc)
        []
        (Stdlib.List.mapi (fun i x -> (i, x)) starts)
      |> Stdlib.List.rev
    in
    { line_start = first; ranges }
  in
  Stdlib.List.map one matches

let clear_highlights m = { m with highlights = []; highlight_index = -1 }

let set_highlights matches m =
  if matches = [] then clear_highlights m
  else
    let highlights = parse_highlights (content m) matches in
    let rec nearest index = function
      | [] -> -1
      | h :: rest ->
          if h.line_start >= m.y_offset then index else nearest (index + 1) rest
    in
    let selected = nearest 0 highlights in
    if selected < 0 then { m with highlights; highlight_index = -1 }
    else
      let h = Stdlib.List.nth highlights selected in
      let colstart, colend =
        match Stdlib.List.find_opt (fun (line, _, _) -> line = h.line_start) h.ranges with
        | Some (_, a, b) -> (a, b)
        | None -> (0, 0)
      in
      ensure_visible ~line:h.line_start ~colstart ~colend
        { m with highlights; highlight_index = selected }

let highlight_next m =
  match m.highlights with
  | [] -> m
  | hs ->
      let i =
        if m.highlight_index < 0 then 0
        else (m.highlight_index + 1) mod Stdlib.List.length hs
      in
      let h = Stdlib.List.nth hs i in
      let colstart, colend =
        match Stdlib.List.find_opt (fun (line, _, _) -> line = h.line_start) h.ranges with
        | Some (_, a, b) -> (a, b)
        | None -> (0, 0)
      in
      ensure_visible ~line:h.line_start ~colstart ~colend { m with highlight_index = i }

let highlight_previous m =
  match m.highlights with
  | [] -> m
  | hs ->
      let i =
        if m.highlight_index <= 0 then Stdlib.List.length hs - 1
        else m.highlight_index - 1
      in
      let h = Stdlib.List.nth hs i in
      let colstart, colend =
        match Stdlib.List.find_opt (fun (line, _, _) -> line = h.line_start) h.ranges with
        | Some (_, a, b) -> (a, b)
        | None -> (0, 0)
      in
      ensure_visible ~line:h.line_start ~colstart ~colend { m with highlight_index = i }

let set_highlight_style style m = { m with highlight_style = style }
let set_selected_highlight_style style m = { m with selected_highlight_style = style }

let key m key =
  if Key_binding.matches key m.keymap.page_down then Some Page_down
  else if Key_binding.matches key m.keymap.page_up then Some Page_up
  else if Key_binding.matches key m.keymap.half_page_up then Some Half_page_up
  else if Key_binding.matches key m.keymap.half_page_down then Some Half_page_down
  else if Key_binding.matches key m.keymap.up then Some Up
  else if Key_binding.matches key m.keymap.down then Some Down
  else if Key_binding.matches key m.keymap.left then Some Left
  else if Key_binding.matches key m.keymap.right then Some Right
  else None

let mouse m (mouse : Charamel_tea.Mouse.t) =
  if not m.mouse_wheel_enabled then None
  else
    match mouse.Charamel_tea.Mouse.button with
    | Charamel_tea.Mouse.Wheel_up | Charamel_tea.Mouse.Wheel_down
    | Charamel_tea.Mouse.Wheel_left | Charamel_tea.Mouse.Wheel_right ->
        Some (Wheel mouse)
    | _ -> None

let update message m =
  let m =
    match message with
    | Page_down -> page_down m
    | Page_up -> page_up m
    | Half_page_up -> half_page_up m
    | Half_page_down -> half_page_down m
    | Up -> scroll_up 1 m
    | Down -> scroll_down 1 m
    | Left -> scroll_left m.horizontal_step m
    | Right -> scroll_right m.horizontal_step m
    | Wheel mouse -> (
        let shift = mouse.Charamel_tea.Mouse.mods.Charamel_tea.Key.shift in
        match mouse.Charamel_tea.Mouse.button with
        | Charamel_tea.Mouse.Wheel_down ->
            if shift then scroll_right m.horizontal_step m
            else scroll_down m.mouse_wheel_delta m
        | Wheel_up ->
            if shift then scroll_left m.horizontal_step m
            else scroll_up m.mouse_wheel_delta m
        | Wheel_left -> scroll_left m.horizontal_step m
        | Wheel_right -> scroll_right m.horizontal_step m
        | _ -> m)
  in
  (m, Cmd.none)

let subscriptions _ = Sub.none
