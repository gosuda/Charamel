type side = [ `Top | `Right | `Bottom | `Left ]

type sides = {
  top : int option;
  right : int option;
  bottom : int option;
  left : int option;
}

let no_sides = { top = None; right = None; bottom = None; left = None }
let side_value = function Some n -> n | None -> 0

let total_sides s =
  if s.top = None && s.right = None && s.bottom = None && s.left = None then None
  else
    Some
      {
        Sides.top = side_value s.top;
        right = side_value s.right;
        bottom = side_value s.bottom;
        left = side_value s.left;
      }

let set_side side value s =
  match side with
  | `Top -> { s with top = Some value }
  | `Right -> { s with right = Some value }
  | `Bottom -> { s with bottom = Some value }
  | `Left -> { s with left = Some value }

let clear_side side s =
  match side with
  | `Top -> { s with top = None }
  | `Right -> { s with right = None }
  | `Bottom -> { s with bottom = None }
  | `Left -> { s with left = None }

let pick_side side s =
  match side with
  | `Top -> s.top
  | `Right -> s.right
  | `Bottom -> s.bottom
  | `Left -> s.left

let is_unset s = s.top = None && s.right = None && s.bottom = None && s.left = None

let check_fill_char name text =
  if text = "" then ()
  else
    match Charamel_ansi.Width.graphemes text with
    | [ _ ] -> ()
    | _ -> invalid_arg (Printf.sprintf "Style.%s: one grapheme is required" name)

type t = {
  bold : bool option;
  italic : bool option;
  underline : bool option;
  underline_style : Charamel_ansi.Style.underline option;
  underline_color : Charamel_ansi.Color.t option;
  strikethrough : bool option;
  reverse : bool option;
  blink : bool option;
  faint : bool option;
  underline_spaces : bool option;
  strikethrough_spaces : bool option;
  color_whitespace : bool option;
  foreground : Charamel_ansi.Color.t option;
  background : Charamel_ansi.Color.t option;
  width : int option;
  height : int option;
  max_width : int option;
  max_height : int option;
  align_horizontal : Position.t option;
  align_vertical : Position.t option;
  padding : sides;
  margin : sides;
  padding_char : string option;
  margin_char : string option;
  margin_background : Charamel_ansi.Color.t option;
  border : Border.t option;
  border_top : bool option;
  border_right : bool option;
  border_bottom : bool option;
  border_left : bool option;
  border_foreground : Sides_color.t option;
  border_background : Sides_color.t option;
  border_foreground_blend : Charamel_ansi.Color.t list option;
  border_foreground_blend_offset : int option;
  inline : bool option;
  tab_width : int option;
  transform : (string -> string) option;
  hyperlink : Charamel_ansi.Link.t option;
}

let empty =
  {
    bold = None;
    italic = None;
    underline = None;
    underline_style = None;
    underline_color = None;
    strikethrough = None;
    reverse = None;
    blink = None;
    faint = None;
    underline_spaces = None;
    strikethrough_spaces = None;
    color_whitespace = None;
    foreground = None;
    background = None;
    width = None;
    height = None;
    max_width = None;
    max_height = None;
    align_horizontal = None;
    align_vertical = None;
    padding = no_sides;
    margin = no_sides;
    padding_char = None;
    margin_char = None;
    margin_background = None;
    border = None;
    border_top = None;
    border_right = None;
    border_bottom = None;
    border_left = None;
    border_foreground = None;
    border_background = None;
    border_foreground_blend = None;
    border_foreground_blend_offset = None;
    inline = None;
    tab_width = None;
    transform = None;
    hyperlink = None;
  }

let bold x t = { t with bold = Some x }
let italic x t = { t with italic = Some x }

let underline x t =
  {
    t with
    underline = Some x;
    underline_style =
      Some (if x then Charamel_ansi.Style.Single else Charamel_ansi.Style.No_underline);
  }

let underline_style x t =
  {
    t with
    underline = Some (x <> Charamel_ansi.Style.No_underline);
    underline_style = Some x;
  }

let underline_color x t = { t with underline_color = Some x }
let strikethrough x t = { t with strikethrough = Some x }
let reverse x t = { t with reverse = Some x }
let blink x t = { t with blink = Some x }
let faint x t = { t with faint = Some x }
let underline_spaces x t = { t with underline_spaces = Some x }
let strikethrough_spaces x t = { t with strikethrough_spaces = Some x }
let color_whitespace x t = { t with color_whitespace = Some x }
let foreground x t = { t with foreground = Some x }
let background x t = { t with background = Some x }
let clamp_size x = max 0 x
let width x t = { t with width = Some (clamp_size x) }
let height x t = { t with height = Some (clamp_size x) }
let max_width x t = { t with max_width = Some (clamp_size x) }
let max_height x t = { t with max_height = Some (clamp_size x) }
let align x t = { t with align_horizontal = Some x; align_vertical = Some x }
let align_horizontal x t = { t with align_horizontal = Some x }
let align_vertical x t = { t with align_vertical = Some x }

let padding x t =
  {
    t with
    padding =
      {
        top = Some x.Sides.top;
        right = Some x.Sides.right;
        bottom = Some x.Sides.bottom;
        left = Some x.Sides.left;
      };
  }

let margin x t =
  {
    t with
    margin =
      {
        top = Some x.Sides.top;
        right = Some x.Sides.right;
        bottom = Some x.Sides.bottom;
        left = Some x.Sides.left;
      };
  }

let padding_side side cells t = { t with padding = set_side side cells t.padding }
let margin_side side cells t = { t with margin = set_side side cells t.margin }

let padding_char text t =
  check_fill_char "padding_char" text;
  { t with padding_char = (if text = "" then None else Some text) }

let margin_char text t =
  check_fill_char "margin_char" text;
  { t with margin_char = (if text = "" then None else Some text) }

let margin_background x t = { t with margin_background = Some x }

let border x t =
  {
    t with
    border = Some x;
    border_top = Some true;
    border_right = Some true;
    border_bottom = Some true;
    border_left = Some true;
  }

let border_top x t = { t with border_top = Some x }
let border_right x t = { t with border_right = Some x }
let border_bottom x t = { t with border_bottom = Some x }
let border_left x t = { t with border_left = Some x }
let border_foreground x t = { t with border_foreground = Some x }
let border_background x t = { t with border_background = Some x }
let border_foreground_blend x t = { t with border_foreground_blend = Some x }

let border_foreground_blend_offset x t =
  { t with border_foreground_blend_offset = Some x }

let inline x t = { t with inline = Some x }
let tab_width x t = { t with tab_width = Some (max (-1) x) }
let transform f t = { t with transform = Some f }
let hyperlink x t = { t with hyperlink = Some x }
let unset_bold t = { t with bold = None }
let unset_italic t = { t with italic = None }
let unset_underline t = { t with underline = None; underline_style = None }
let unset_underline_style t = { t with underline_style = None }
let unset_underline_color t = { t with underline_color = None }
let unset_strikethrough t = { t with strikethrough = None }
let unset_reverse t = { t with reverse = None }
let unset_blink t = { t with blink = None }
let unset_faint t = { t with faint = None }
let unset_underline_spaces t = { t with underline_spaces = None }
let unset_strikethrough_spaces t = { t with strikethrough_spaces = None }
let unset_color_whitespace t = { t with color_whitespace = None }
let unset_foreground t = { t with foreground = None }
let unset_background t = { t with background = None }
let unset_width t = { t with width = None }
let unset_height t = { t with height = None }
let unset_max_width t = { t with max_width = None }
let unset_max_height t = { t with max_height = None }
let unset_align t = { t with align_horizontal = None; align_vertical = None }
let unset_align_horizontal t = { t with align_horizontal = None }
let unset_align_vertical t = { t with align_vertical = None }
let unset_padding t = { t with padding = no_sides }
let unset_margin t = { t with margin = no_sides }
let unset_padding_side side t = { t with padding = clear_side side t.padding }
let unset_margin_side side t = { t with margin = clear_side side t.margin }
let unset_padding_char t = { t with padding_char = None }
let unset_margin_char t = { t with margin_char = None }
let unset_margin_background t = { t with margin_background = None }

let unset_border t =
  {
    t with
    border = None;
    border_top = None;
    border_right = None;
    border_bottom = None;
    border_left = None;
  }

let unset_border_top t = { t with border_top = None }
let unset_border_right t = { t with border_right = None }
let unset_border_bottom t = { t with border_bottom = None }
let unset_border_left t = { t with border_left = None }
let unset_border_foreground t = { t with border_foreground = None }
let unset_border_background t = { t with border_background = None }
let unset_border_foreground_blend t = { t with border_foreground_blend = None }

let unset_border_foreground_blend_offset t =
  { t with border_foreground_blend_offset = None }

let unset_inline t = { t with inline = None }
let unset_tab_width t = { t with tab_width = None }
let unset_transform t = { t with transform = None }
let unset_hyperlink t = { t with hyperlink = None }
let choose a b = match a with Some _ -> a | None -> b

let inherit_ ~parent child =
  let underline, underline_style =
    match (child.underline, child.underline_style) with
    | Some value, style -> (Some value, style)
    | None, Some value -> (None, Some value)
    | None, None -> (parent.underline, parent.underline_style)
  in
  {
    bold = choose child.bold parent.bold;
    italic = choose child.italic parent.italic;
    underline;
    underline_style;
    underline_color = choose child.underline_color parent.underline_color;
    strikethrough = choose child.strikethrough parent.strikethrough;
    reverse = choose child.reverse parent.reverse;
    blink = choose child.blink parent.blink;
    faint = choose child.faint parent.faint;
    underline_spaces = choose child.underline_spaces parent.underline_spaces;
    strikethrough_spaces = choose child.strikethrough_spaces parent.strikethrough_spaces;
    color_whitespace = choose child.color_whitespace parent.color_whitespace;
    foreground = choose child.foreground parent.foreground;
    background = choose child.background parent.background;
    width = choose child.width parent.width;
    height = choose child.height parent.height;
    max_width = choose child.max_width parent.max_width;
    max_height = choose child.max_height parent.max_height;
    align_horizontal = choose child.align_horizontal parent.align_horizontal;
    align_vertical = choose child.align_vertical parent.align_vertical;
    padding = child.padding;
    margin = child.margin;
    padding_char = child.padding_char;
    margin_char = child.margin_char;
    margin_background =
      choose child.margin_background
        (match parent.margin_background with
        | Some _ -> parent.margin_background
        | None -> parent.background);
    border = choose child.border parent.border;
    border_top = choose child.border_top parent.border_top;
    border_right = choose child.border_right parent.border_right;
    border_bottom = choose child.border_bottom parent.border_bottom;
    border_left = choose child.border_left parent.border_left;
    border_foreground = choose child.border_foreground parent.border_foreground;
    border_background = choose child.border_background parent.border_background;
    border_foreground_blend =
      choose child.border_foreground_blend parent.border_foreground_blend;
    border_foreground_blend_offset =
      choose child.border_foreground_blend_offset parent.border_foreground_blend_offset;
    inline = choose child.inline parent.inline;
    tab_width = choose child.tab_width parent.tab_width;
    transform = choose child.transform parent.transform;
    hyperlink = child.hyperlink;
  }

let get_width t = t.width
let get_height t = t.height
let get_max_width t = t.max_width
let get_max_height t = t.max_height
let get_padding t = total_sides t.padding
let get_margin t = total_sides t.margin
let get_padding_side side t = pick_side side t.padding
let get_margin_side side t = pick_side side t.margin
let get_padding_char t = t.padding_char
let get_margin_char t = t.margin_char
let get_foreground t = t.foreground
let get_background t = t.background
let get_border t = t.border
let get_align_horizontal t = t.align_horizontal
let get_align_vertical t = t.align_vertical
let get_tab_width t = t.tab_width
let get_hyperlink t = t.hyperlink
let get_border_foreground t = t.border_foreground
let get_border_background t = t.border_background
let get_border_foreground_blend t = t.border_foreground_blend
let get_border_foreground_blend_offset t = t.border_foreground_blend_offset

let apply_ansi style text =
  if Charamel_ansi.Style.equal style Charamel_ansi.Style.default then text
  else Charamel_ansi.Style.to_sgr style ^ text ^ "\x1b[m"

let fill_char = function Some c -> c | None -> " "

let pad_fill char style cells =
  if cells <= 0 then ""
  else apply_ansi style (Whitespace.fill ~pattern:(fill_char char) cells)

let resolve_underline t =
  match (t.underline, t.underline_style) with
  | Some false, _ -> Charamel_ansi.Style.No_underline
  | _, Some u -> u
  | _ ->
      if Option.value ~default:false t.underline then Charamel_ansi.Style.Single
      else Charamel_ansi.Style.No_underline

let underline_active t = resolve_underline t <> Charamel_ansi.Style.No_underline

let ansi_style t =
  {
    Charamel_ansi.Style.fg =
      (match t.foreground with Some c -> c | None -> Charamel_ansi.Color.Default);
    bg = (match t.background with Some c -> c | None -> Charamel_ansi.Color.Default);
    underline_color =
      (match t.underline_color with Some c -> c | None -> Charamel_ansi.Color.Default);
    bold = Option.value ~default:false t.bold;
    faint = Option.value ~default:false t.faint;
    italic = Option.value ~default:false t.italic;
    underline = resolve_underline t;
    blink = Option.value ~default:false t.blink;
    reverse = Option.value ~default:false t.reverse;
    conceal = false;
    strike = Option.value ~default:false t.strikethrough;
  }

let first_scalar s =
  if s = "" then Uchar.of_int 0 else Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0)

let is_space s = Uucp.White.is_white_space (first_scalar s)

type escape_state = Ground | Escape | Csi | Dcs | String_payload of bool

let escape_spans line =
  let n = String.length line in
  let spans = ref [] in
  let state = ref Ground in
  let raw = ref true in
  let start = ref 0 in
  let emit stop =
    if stop > !start then
      let span = String.sub line !start (stop - !start) in
      spans := (if !raw then `Text span else `Escape span) :: !spans
  in
  let scalar_length i =
    let decoded = String.get_utf_8_uchar line i in
    if Uchar.utf_decode_is_valid decoded then Uchar.utf_decode_length decoded else 1
  in
  let i = ref 0 in
  while !i < n do
    let byte = Char.code line.[!i] in
    let count = if byte >= 0xc2 then scalar_length !i else 1 in
    let next_raw, next_state =
      match !state with
      | String_payload osc ->
          if count > 1 then (false, !state)
          else if byte = 0x1b then (false, Escape)
          else if byte = 0x9c || byte = 0x18 || byte = 0x1a || (osc && byte = 0x07) then
            (false, Ground)
          else (false, !state)
      | _ when byte = 0x1b -> (false, Escape)
      | _ when count > 1 -> (true, Ground)
      | _ when byte = 0x9b -> (false, Csi)
      | _ when byte = 0x90 -> (false, Dcs)
      | _ when byte = 0x9d -> (false, String_payload true)
      | _ when byte = 0x98 || byte = 0x9e || byte = 0x9f -> (false, String_payload false)
      | _ when byte = 0x18 || byte = 0x1a -> (true, Ground)
      | Ground -> (true, Ground)
      | Escape ->
          if byte = 0x5b then (false, Csi)
          else if byte = 0x50 then (false, Dcs)
          else if byte = 0x5d then (false, String_payload true)
          else if byte = 0x58 || byte = 0x5e || byte = 0x5f then
            (false, String_payload false)
          else if byte < 0x20 then (true, Escape)
          else if byte >= 0x30 && byte <= 0x7e then (false, Ground)
          else (false, Escape)
      | Csi ->
          if byte < 0x20 then (true, Csi)
          else if byte >= 0x40 && byte <= 0x7e then (false, Ground)
          else (false, Csi)
      | Dcs ->
          if byte >= 0x40 && byte <= 0x7e then (false, String_payload false)
          else (false, Dcs)
    in
    if next_raw <> !raw then begin
      emit !i;
      start := !i;
      raw := next_raw
    end;
    state := next_state;
    i := !i + count
  done;
  emit n;
  Stdlib.List.rev !spans

let render_line t te te_space line =
  let needs_spaces =
    Option.value ~default:false t.underline_spaces
    || Option.value ~default:false t.strikethrough_spaces
    || underline_active t
    || Option.value ~default:false t.strikethrough
  in
  if not needs_spaces then apply_ansi te line
  else begin
    let out = Buffer.create (String.length line + 16) in
    Stdlib.List.iter
      (function
        | `Escape raw -> Buffer.add_string out raw
        | `Text text ->
            Stdlib.List.iter
              (fun g ->
                let style = if is_space g then te_space else te in
                Buffer.add_string out (apply_ansi style g))
              (Charamel_ansi.Width.graphemes text))
      (escape_spans line);
    Buffer.contents out
  end

let replace_tabs width s =
  if width = -1 then s
  else begin
    let replacement = if width <= 0 then "" else String.make width ' ' in
    let out = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        if c = '\t' then Buffer.add_string out replacement else Buffer.add_char out c)
      s;
    Buffer.contents out
  end

let lines s = String.split_on_char '\n' s
let join_lines = String.concat "\n"
let line_width = Charamel_ansi.Text.width
let max_line_width xs = Stdlib.List.fold_left (fun m x -> max m (line_width x)) 0 xs
let spaces n = String.make (max 0 n) ' '
let styled_pad style n = if n <= 0 then "" else apply_ansi style (spaces n)

let align_lines_vertical pos h ls =
  let n = Stdlib.List.length ls in
  if h <= n then ls
  else
    let gap = h - n in
    let top, bottom =
      if pos = Position.top then (0, gap)
      else if pos = Position.bottom then (gap, 0)
      else
        let share, rest = Position.split pos gap in
        (rest, share)
    in
    Stdlib.List.init top (fun _ -> "") @ ls @ Stdlib.List.init bottom (fun _ -> "")

let align_lines_horizontal pos target te ls =
  let widest = max_line_width ls in
  let target = max widest target in
  Stdlib.List.map
    (fun line ->
      let gap = target - line_width line in
      if gap <= 0 then line
      else if pos = Position.left then line ^ styled_pad te gap
      else if pos = Position.right then styled_pad te gap ^ line
      else
        let share, rest = Position.split pos gap in
        styled_pad te rest ^ line ^ styled_pad te share)
    ls

let border_enabled t side =
  let all_unspecified =
    Option.is_none t.border_top && Option.is_none t.border_right
    && Option.is_none t.border_bottom
    && Option.is_none t.border_left
  in
  match (t.border, side) with
  | None, _ -> false
  | Some _, `Top -> Option.value ~default:all_unspecified t.border_top
  | Some _, `Right -> Option.value ~default:all_unspecified t.border_right
  | Some _, `Bottom -> Option.value ~default:all_unspecified t.border_bottom
  | Some _, `Left -> Option.value ~default:all_unspecified t.border_left

let first_glyph s = match Charamel_ansi.Width.graphemes s with g :: _ -> g | [] -> " "

let max_glyph_width s =
  match Charamel_ansi.Width.graphemes s with
  | [] -> 1
  | gs ->
      Stdlib.List.fold_left
        (fun width glyph -> max width (Charamel_ansi.Width.grapheme_width glyph))
        0 gs

let border_edge_width (b : Border.t) side ~top ~bottom =
  let parts =
    match side with
    | `Left ->
        (b.Border.left :: (if top then [ b.Border.top_left ] else []))
        @ if bottom then [ b.Border.bottom_left ] else []
    | `Right ->
        (b.Border.right :: (if top then [ b.Border.top_right ] else []))
        @ if bottom then [ b.Border.bottom_right ] else []
    | `Top -> [ b.Border.top ]
    | `Bottom -> [ b.Border.bottom ]
  in
  Stdlib.List.fold_left (fun width part -> max width (max_glyph_width part)) 0 parts

let border_dimensions t =
  match t.border with
  | None -> (0, 0)
  | Some b when b = Border.none -> (0, 0)
  | Some b ->
      let top = border_enabled t `Top
      and right = border_enabled t `Right
      and bottom = border_enabled t `Bottom
      and left = border_enabled t `Left in
      let horizontal =
        (if left then border_edge_width b `Left ~top ~bottom else 0)
        + if right then border_edge_width b `Right ~top ~bottom else 0
      in
      let vertical =
        (if top then border_edge_width b `Top ~top ~bottom else 0)
        + if bottom then border_edge_width b `Bottom ~top ~bottom else 0
      in
      (horizontal, vertical)

let get_align t = t.align_horizontal

let get_border_top_size t =
  match t.border with
  | None -> 0
  | Some b when b = Border.none -> 0
  | Some b -> if border_enabled t `Top then Border.top_size b else 0

let get_border_right_size t =
  match t.border with
  | None -> 0
  | Some b when b = Border.none -> 0
  | Some b -> if border_enabled t `Right then Border.right_size b else 0

let get_border_bottom_size t =
  match t.border with
  | None -> 0
  | Some b when b = Border.none -> 0
  | Some b -> if border_enabled t `Bottom then Border.bottom_size b else 0

let get_border_left_size t =
  match t.border with
  | None -> 0
  | Some b when b = Border.none -> 0
  | Some b -> if border_enabled t `Left then Border.left_size b else 0

let get_horizontal_border_size t = get_border_left_size t + get_border_right_size t
let get_vertical_border_size t = get_border_top_size t + get_border_bottom_size t
let get_horizontal_padding t = side_value t.padding.left + side_value t.padding.right
let get_vertical_padding t = side_value t.padding.top + side_value t.padding.bottom
let get_horizontal_margins t = side_value t.margin.left + side_value t.margin.right
let get_vertical_margins t = side_value t.margin.top + side_value t.margin.bottom

let get_horizontal_frame_size t =
  get_horizontal_padding t + get_horizontal_margins t + get_horizontal_border_size t

let get_vertical_frame_size t =
  get_vertical_padding t + get_vertical_margins t + get_vertical_border_size t

let get_frame_size t = (get_horizontal_frame_size t, get_vertical_frame_size t)
let no_tab_conversion = -1
let nbsp = "\u{00A0}"

let side_color (sides : Sides_color.t option) side =
  match (sides, side) with
  | None, _ -> None
  | Some s, `Top -> s.Sides_color.top
  | Some s, `Right -> s.Sides_color.right
  | Some s, `Bottom -> s.Sides_color.bottom
  | Some s, `Left -> s.Sides_color.left

let edge_background t side =
  match side_color t.border_background side with
  | Some c -> c
  | None -> Charamel_ansi.Color.Default

let color_text ?fg t side text =
  let foreground =
    match fg with
    | Some c -> c
    | None -> (
        match side_color t.border_foreground side with
        | Some c -> c
        | None -> Charamel_ansi.Color.Default)
  in
  apply_ansi
    { Charamel_ansi.Style.default with fg = foreground; bg = edge_background t side }
    text

type border_slices = {
  top_g : Charamel_ansi.Color.t array;
  right_g : Charamel_ansi.Color.t array;
  bottom_g : Charamel_ansi.Color.t array;
  left_g : Charamel_ansi.Color.t array;
}

let rotate xs offset =
  let n = Stdlib.List.length xs in
  if n = 0 then xs
  else
    let r = offset mod n in
    let r = if r < 0 then r + n else r in
    if r = 0 then xs
    else
      let a = Array.of_list xs in
      Array.to_list Array.(append (sub a r (n - r)) (sub a 0 r))

let border_blend t ~width ~height =
  match t.border_foreground_blend with
  | Some (_ :: _ as colors) ->
      let offset = Option.value ~default:0 t.border_foreground_blend_offset in
      let steps = (height + width + 2) * 2 in
      let rotated = rotate (Blending.blend1d ~steps colors) (-offset) in
      let take n xs = Stdlib.List.filteri (fun i _ -> i < n) xs in
      let drop n xs = Stdlib.List.filteri (fun i _ -> i >= n) xs in
      let top_slice = take (width + 2) rotated in
      let after_top = drop (width + 2) rotated in
      let right_slice = take height after_top in
      let after_right = drop height after_top in
      let bottom_slice = Stdlib.List.rev (take (width + 2) after_right) in
      let after_bottom = drop (width + 2) after_right in
      let left_slice = Stdlib.List.rev (take height after_bottom) in
      Some
        {
          top_g = Array.of_list top_slice;
          right_g = Array.of_list right_slice;
          bottom_g = Array.of_list bottom_slice;
          left_g = Array.of_list left_slice;
        }
  | _ -> None

let blend_text slice background text =
  let glyphs = Charamel_ansi.Width.graphemes text in
  let last = Array.length slice - 1 in
  let out = Buffer.create (String.length text + (32 * Stdlib.List.length glyphs)) in
  Stdlib.List.iteri
    (fun i glyph ->
      let fg = if i <= last then slice.(i) else Charamel_ansi.Color.Default in
      Buffer.add_string out
        (Charamel_ansi.Style.to_sgr
           { Charamel_ansi.Style.default with fg; bg = background });
      Buffer.add_string out glyph)
    glyphs;
  Buffer.add_string out "\x1b[m";
  Buffer.contents out

let edge_text t blend ~top_line text =
  let side = if top_line then `Top else `Bottom in
  match blend with
  | None -> color_text t side text
  | Some slices ->
      let slice = if top_line then slices.top_g else slices.bottom_g in
      blend_text slice (edge_background t side) text

let side_text t blend side i glyph =
  match blend with
  | None -> color_text t side glyph
  | Some slices ->
      let slice = if side = `Left then slices.left_g else slices.right_g in
      let fg = slice.(min i (Array.length slice - 1)) in
      color_text ~fg t side glyph

let apply_border t ls =
  match t.border with
  | None -> ls
  | Some b when b = Border.none -> ls
  | Some b ->
      let top = border_enabled t `Top
      and right = border_enabled t `Right
      and bottom = border_enabled t `Bottom
      and left = border_enabled t `Left in
      if not (top || right || bottom || left) then ls
      else
        let content_width = max_line_width ls in
        let left_w = if left then border_edge_width b `Left ~top ~bottom else 0
        and right_w = if right then border_edge_width b `Right ~top ~bottom else 0 in
        let blend =
          border_blend t
            ~width:(content_width + left_w + right_w)
            ~height:(Stdlib.List.length ls)
        in
        let cycle_glyphs s =
          match Charamel_ansi.Width.graphemes s with [] -> [ " " ] | gs -> gs
        in
        let fill_edge gs target_width =
          if
            target_width <= 0
            || not
                 (Stdlib.List.exists
                    (fun g -> Charamel_ansi.Width.grapheme_width g > 0)
                    gs)
          then ""
          else
            let out = Buffer.create target_width in
            let n = Stdlib.List.length gs in
            let i = ref 0 and columns = ref 0 and fits = ref true in
            while !fits do
              let g = Stdlib.List.nth gs (!i mod n) in
              let w = Charamel_ansi.Width.grapheme_width g in
              if w <= 0 then begin
                Buffer.add_string out g;
                incr i
              end
              else if !columns + w <= target_width then begin
                Buffer.add_string out g;
                columns := !columns + w;
                incr i
              end
              else fits := false
            done;
            Buffer.add_string out (spaces (target_width - !columns));
            Buffer.contents out
        in
        let fit_edge side width glyph =
          let missing = width - line_width glyph in
          if missing <= 0 then glyph
          else if side = `Left then glyph ^ spaces missing
          else spaces missing ^ glyph
        in
        let corner side top_line =
          let glyph =
            if side = `Left then
              first_glyph (if top_line then b.Border.top_left else b.Border.bottom_left)
            else
              first_glyph (if top_line then b.Border.top_right else b.Border.bottom_right)
          in
          fit_edge side (if side = `Left then left_w else right_w) glyph
        in
        let hline ~top_line =
          let mid = if top_line then b.Border.top else b.Border.bottom in
          let text =
            (if left then corner `Left top_line else "")
            ^ fill_edge (cycle_glyphs mid) content_width
            ^ if right then corner `Right top_line else ""
          in
          edge_text t blend ~top_line text
        in
        let left_glyphs = cycle_glyphs b.Border.left
        and right_glyphs = cycle_glyphs b.Border.right in
        let side_glyph side i =
          let gs = if side = `Left then left_glyphs else right_glyphs in
          let glyph = Stdlib.List.nth gs (i mod Stdlib.List.length gs) in
          fit_edge side (if side = `Left then left_w else right_w) glyph
        in
        let top_line = if top then [ hline ~top_line:true ] else [] in
        let body =
          Stdlib.List.mapi
            (fun i line ->
              let line = line ^ spaces (content_width - line_width line) in
              let l =
                if left then side_text t blend `Left i (side_glyph `Left i) else ""
              in
              let r =
                if right then side_text t blend `Right i (side_glyph `Right i) else ""
              in
              l ^ line ^ r)
            ls
        in
        let bottom_line = if bottom then [ hline ~top_line:false ] else [] in
        top_line @ body @ bottom_line

let apply_margins t ls =
  let m = t.margin in
  if is_unset m then ls
  else
    let margin_style =
      match t.margin_background with
      | None -> Charamel_ansi.Style.default
      | Some c -> { Charamel_ansi.Style.default with bg = c }
    in
    let left = side_value m.left and right = side_value m.right in
    let body =
      Stdlib.List.map
        (fun line ->
          pad_fill t.margin_char margin_style left
          ^ line
          ^ pad_fill t.margin_char margin_style right)
        ls
    in
    let blank = pad_fill t.margin_char margin_style (max_line_width ls + left + right) in
    Stdlib.List.init (side_value m.top) (fun _ -> blank)
    @ body
    @ Stdlib.List.init (side_value m.bottom) (fun _ -> blank)

let render t input =
  let input = match t.transform with None -> input | Some f -> f input in
  let input = replace_tabs (Option.value ~default:4 t.tab_width) input in
  if
    Option.is_none t.transform && Option.is_none t.bold && Option.is_none t.italic
    && Option.is_none t.underline
    && Option.is_none t.underline_style
    && Option.is_none t.underline_color
    && Option.is_none t.strikethrough
    && Option.is_none t.reverse && Option.is_none t.blink && Option.is_none t.faint
    && Option.is_none t.underline_spaces
    && Option.is_none t.strikethrough_spaces
    && Option.is_none t.color_whitespace
    && Option.is_none t.foreground && Option.is_none t.background
    && Option.is_none t.width && Option.is_none t.height && Option.is_none t.max_width
    && Option.is_none t.max_height
    && Option.is_none t.align_horizontal
    && Option.is_none t.align_vertical
    && is_unset t.padding && is_unset t.margin && Option.is_none t.padding_char
    && Option.is_none t.margin_char
    && Option.is_none t.margin_background
    && Option.is_none t.border && Option.is_none t.border_top
    && Option.is_none t.border_right
    && Option.is_none t.border_bottom
    && Option.is_none t.border_left
    && Option.is_none t.border_foreground
    && Option.is_none t.border_background
    && Option.is_none t.border_foreground_blend
    && Option.is_none t.border_foreground_blend_offset
    && Option.is_none t.inline && Option.is_none t.tab_width && Option.is_none t.hyperlink
  then input
  else
    let inline = Option.value ~default:false t.inline in
    let input =
      let b = Buffer.create (String.length input) in
      let rec loop i =
        if i >= String.length input then ()
        else if i + 1 < String.length input && input.[i] = '\r' && input.[i + 1] = '\n'
        then (
          Buffer.add_char b '\n';
          loop (i + 2))
        else (
          Buffer.add_char b input.[i];
          loop (i + 1))
      in
      loop 0;
      Buffer.contents b
    in
    let input =
      if inline then String.concat "" (String.split_on_char '\n' input) else input
    in
    let padding = t.padding in
    let border_h, border_v = border_dimensions t in
    let requested_width = Option.value ~default:0 t.width - border_h in
    let requested_height = Option.value ~default:0 t.height - border_v in
    let input =
      if (not inline) && requested_width > 0 then
        Charamel_ansi.Text.wrap
          ~width:(requested_width - side_value padding.left - side_value padding.right)
          input
      else input
    in
    let te = ansi_style t in
    let is_reverse = Option.value ~default:false t.reverse in
    let color_spaces = Option.value ~default:true t.color_whitespace in
    let use_space_styler =
      underline_active t
      || Option.value ~default:false t.strikethrough
      || Option.value ~default:false t.underline_spaces
      || Option.value ~default:false t.strikethrough_spaces
    in
    let color_or_default = function Some c -> c | None -> Charamel_ansi.Color.Default in
    let te_whitespace =
      {
        Charamel_ansi.Style.default with
        reverse = is_reverse;
        fg =
          (if is_reverse then color_or_default t.foreground
           else Charamel_ansi.Color.Default);
        bg =
          (if color_spaces then color_or_default t.background
           else Charamel_ansi.Color.Default);
        underline_color =
          (if color_spaces then color_or_default t.underline_color
           else Charamel_ansi.Color.Default);
      }
    in
    let underline_spaces_effective =
      match t.underline_spaces with Some x -> x | None -> underline_active t
    in
    let strikethrough_spaces_effective =
      match t.strikethrough_spaces with
      | Some x -> x
      | None -> Option.value ~default:false t.strikethrough
    in
    let te_space =
      {
        Charamel_ansi.Style.default with
        fg =
          (if use_space_styler then color_or_default t.foreground
           else Charamel_ansi.Color.Default);
        bg =
          (if use_space_styler then color_or_default t.background
           else Charamel_ansi.Color.Default);
        underline_color =
          (if use_space_styler then color_or_default t.underline_color
           else Charamel_ansi.Color.Default);
        underline =
          (if underline_spaces_effective then Charamel_ansi.Style.Single
           else Charamel_ansi.Style.No_underline);
        strike = strikethrough_spaces_effective;
      }
    in
    let rendered = Stdlib.List.map (render_line t te te_space) (lines input) in
    let rendered =
      match (t.hyperlink, rendered) with
      | None, _ -> rendered
      | Some _, [] -> []
      | Some link, [ line ] ->
          [ Charamel_ansi.Link.osc8 (Some link) ^ line ^ Charamel_ansi.Link.osc8 None ]
      | Some link, lines ->
          let open_link = Charamel_ansi.Link.osc8 (Some link) in
          let close_link = Charamel_ansi.Link.osc8 None in
          let last = Stdlib.List.length lines - 1 in
          Stdlib.List.mapi
            (fun index line ->
              if index = 0 then open_link ^ line
              else if index = last then line ^ close_link
              else line)
            lines
    in
    let rendered =
      if inline then rendered
      else
        Stdlib.List.map
          (fun line ->
            pad_fill t.padding_char te_whitespace (side_value padding.left)
            ^ line
            ^ pad_fill t.padding_char te_whitespace (side_value padding.right))
          rendered
    in
    let rendered =
      if inline then rendered
      else
        let blank = pad_fill t.padding_char te_whitespace (max_line_width rendered) in
        Stdlib.List.init (side_value padding.top) (fun _ -> blank)
        @ rendered
        @ Stdlib.List.init (side_value padding.bottom) (fun _ -> blank)
    in
    let rendered =
      align_lines_vertical
        (Option.value ~default:Position.top t.align_vertical)
        requested_height rendered
    in
    let rendered =
      if requested_width <> 0 || Stdlib.List.length rendered > 1 then
        align_lines_horizontal
          (Option.value ~default:Position.left t.align_horizontal)
          requested_width te_whitespace rendered
      else rendered
    in
    let rendered = if inline then rendered else apply_border t rendered in
    let rendered = if inline then rendered else apply_margins t rendered in
    let rendered =
      match t.max_width with
      | Some n when n > 0 ->
          Stdlib.List.map (Charamel_ansi.Text.truncate ~width:n) rendered
      | None | Some _ -> rendered
    in
    let rendered =
      match t.max_height with
      | Some n when n > 0 -> Stdlib.List.filteri (fun i _ -> i < n) rendered
      | None | Some _ -> rendered
    in
    join_lines rendered

let get_bold t = t.bold
let get_italic t = t.italic

let get_underline t =
  match t.underline with
  | Some value -> Some value
  | None ->
      Option.map
        (fun style -> style <> Charamel_ansi.Style.No_underline)
        t.underline_style

let get_underline_style t = t.underline_style
let get_underline_color t = t.underline_color
let get_strikethrough t = t.strikethrough
let get_reverse t = t.reverse
let get_blink t = t.blink
let get_faint t = t.faint
let get_underline_spaces t = t.underline_spaces
let get_strikethrough_spaces t = t.strikethrough_spaces
let get_color_whitespace t = t.color_whitespace
let get_margin_background t = t.margin_background
let get_border_top t = t.border_top
let get_border_right t = t.border_right
let get_border_bottom t = t.border_bottom
let get_border_left t = t.border_left
let get_inline t = t.inline
let get_transform t = t.transform
