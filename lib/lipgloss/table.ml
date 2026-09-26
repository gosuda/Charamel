type row = string list
type style_func = row:int -> col:int -> Style.t

module Data = struct
  type t = { content : string list list }

  let rows content = { content }

  let at t ~row ~col =
    match Stdlib.List.nth_opt t.content row with
    | None -> ""
    | Some cells -> (
        match Stdlib.List.nth_opt cells col with Some cell -> cell | None -> "")

  let row_count t = Stdlib.List.length t.content

  let columns t =
    Stdlib.List.fold_left (fun n row -> max n (Stdlib.List.length row)) 0 t.content

  let append t row = { content = t.content @ [ row ] }
  let filter t keep = { content = Stdlib.List.filteri (fun i _ -> keep i) t.content }
  let matrix t = t.content
end

type t = {
  headers : string list;
  data : Data.t;
  border : Border.t;
  border_top : bool;
  border_bottom : bool;
  border_left : bool;
  border_right : bool;
  border_header : bool;
  border_column : bool;
  border_row : bool;
  base_style : Style.t;
  border_style : Style.t option;
  style : style_func option;
  width : int option;
  fit_content : bool;
  height : int option;
  offset : int;
  wrap : bool;
}

type column = {
  mutable min_width : int;
  mutable max_width : int;
  mutable x_frame : int;
  mutable fixed_width : int;
  mutable median_values : int list;
}

let v ?(headers = []) ?data ?(rows = []) ?(border = Border.normal) ?(border_top = true)
    ?(border_bottom = true) ?(border_left = true) ?(border_right = true)
    ?(border_header = true) ?(border_column = true) ?(border_row = false)
    ?(base_style = Style.empty) ?border_style ?style ?width ?(fit_content = false) ?height
    ?(offset = 0) ?(wrap = true) () =
  let data = match data with Some data -> data | None -> Data.rows rows in
  {
    headers;
    data;
    border;
    border_top;
    border_bottom;
    border_left;
    border_right;
    border_header;
    border_column;
    border_row;
    base_style;
    border_style;
    style;
    width;
    fit_content;
    height;
    offset = max 0 offset;
    wrap;
  }

let data t = t.data
let headers t = t.headers
let height t = Option.value t.height ~default:0
let y_offset t = t.offset

let nth_default ~default xs i =
  match Stdlib.List.nth_opt xs i with Some x -> x | None -> default

let median values =
  match Stdlib.List.sort compare values with
  | [] -> 0
  | xs ->
      let length = Stdlib.List.length xs in
      let middle = length / 2 in
      if length mod 2 = 0 then
        (Stdlib.List.nth xs (middle - 1) + Stdlib.List.nth xs middle) / 2
      else Stdlib.List.nth xs middle

let side_values style =
  let padding = Option.value (Style.get_padding style) ~default:(Sides.v ()) in
  let margin = Option.value (Style.get_margin style) ~default:(Sides.v ()) in
  let border = Style.get_border style in
  let enabled getter = Option.value (getter style) ~default:false in
  let edge_size side =
    match border with
    | None -> 0
    | Some b when b = Border.none -> 0
    | Some b ->
        let on =
          enabled
            (match side with
            | `Left -> Style.get_border_left
            | `Right -> Style.get_border_right
            | `Top -> Style.get_border_top
            | `Bottom -> Style.get_border_bottom)
        in
        if not on then 0
        else
          max 1
            (match side with
            | `Left -> Border.left_size b
            | `Right -> Border.right_size b
            | `Top -> Border.top_size b
            | `Bottom -> Border.bottom_size b)
  in
  let horizontal =
    padding.Sides.left + padding.Sides.right + margin.Sides.left + margin.Sides.right
    + edge_size `Left + edge_size `Right
  in
  let vertical =
    padding.Sides.top + padding.Sides.bottom + margin.Sides.top + margin.Sides.bottom
    + edge_size `Top + edge_size `Bottom
  in
  (horizontal, vertical, margin)

let frame_width style =
  let h, _, _ = side_values style in
  h

let frame_height style =
  let _, v, _ = side_values style in
  v

let style_get_width style = Option.value (Style.get_width style) ~default:0
let style_get_height style = Option.value (Style.get_height style) ~default:1
let pad_to n xs = xs @ Stdlib.List.init (max 0 (n - Stdlib.List.length xs)) (fun _ -> "")

let repeat_glyph glyph width =
  let glyph_width = Charamel_ansi.Text.width glyph in
  if width <= 0 then ""
  else if glyph = "" || glyph_width <= 0 then String.make width ' '
  else
    let count = width / glyph_width in
    let remainder = width - (count * glyph_width) in
    String.concat "" (Stdlib.List.init count (fun _ -> glyph)) ^ String.make remainder ' '

let widest glyphs =
  Stdlib.List.fold_left (fun n glyph -> max n (Charamel_ansi.Text.width glyph)) 0 glyphs

let border_budget (border : Border.t) ~left ~right ~column columns =
  if columns = 0 then 0
  else
    let left_edge =
      if left then
        widest
          [ border.Border.left; border.top_left; border.middle_left; border.bottom_left ]
      else 0
    in
    let right_edge =
      if right then
        widest
          [ border.right; border.top_right; border.middle_right; border.bottom_right ]
      else 0
    in
    let between =
      if column then
        widest [ border.left; border.middle_top; border.middle; border.middle_bottom ]
      else 0
    in
    left_edge + right_edge + max 0 ((columns - 1) * between)

let horizontal_line widths ~left ~right ~middle ~segment ~paint =
  let b = Buffer.create 64 in
  let count = Array.length widths in
  if left <> "" then Buffer.add_string b (paint left);
  for i = 0 to count - 1 do
    Buffer.add_string b (paint (repeat_glyph segment widths.(i)));
    if i < count - 1 && middle <> "" then Buffer.add_string b (paint middle)
  done;
  if right <> "" then Buffer.add_string b (paint right);
  Buffer.contents b

let border_line_present a b c d = a <> "" || b <> "" || c <> "" || d <> ""

let style_for t has_headers row col =
  match t.style with
  | None -> t.base_style
  | Some f ->
      Style.inherit_ ~parent:t.base_style
        (f ~row:(if has_headers then row - 1 else row) ~col)

let border_style t =
  match t.border_style with
  | None -> t.base_style
  | Some style -> Style.inherit_ ~parent:t.base_style style

let normalize_newlines content =
  let b = Buffer.create (String.length content) in
  let rec loop i =
    if i >= String.length content then ()
    else if i + 1 < String.length content && content.[i] = '\r' && content.[i + 1] = '\n'
    then (
      Buffer.add_char b '\n';
      loop (i + 2))
    else (
      Buffer.add_char b content.[i];
      loop (i + 1))
  in
  loop 0;
  Buffer.contents b

let line_height content =
  Stdlib.List.length (String.split_on_char '\n' (normalize_newlines content))

let wrapped_height ~width content =
  let normalized = normalize_newlines content in
  if width < 1 then 1
  else
    String.split_on_char '\n' normalized
    |> Stdlib.List.fold_left
         (fun n line ->
           n
           + Stdlib.List.length
               (String.split_on_char '\n' (Charamel_ansi.Text.wrap ~width line)))
         0

let shrink_widths columns ~border_width target =
  let widths =
    Array.map
      (fun c -> if c.fixed_width > 0 then c.fixed_width else c.max_width + c.x_frame)
      columns
  in
  let total () = Array.fold_left ( + ) border_width widths in
  let min_width i = columns.(i).x_frame + max columns.(i).min_width 1 in
  let decrement ~very_big ~floor =
    let rec loop () =
      if total () <= target then ()
      else
        let chosen = ref (-1) and chosen_width = ref min_int in
        Array.iteri
          (fun i width ->
            if
              columns.(i).fixed_width = 0
              && ((not floor) || width > min_width i)
              && ((not very_big) || width >= target / 2)
              && width > !chosen_width
            then (
              chosen := i;
              chosen_width := width))
          widths;
        if !chosen < 0 then ()
        else (
          widths.(!chosen) <- widths.(!chosen) - 1;
          loop ())
    in
    loop ()
  in
  let toward_median ~floor =
    let rec loop () =
      if total () <= target then ()
      else
        let chosen = ref (-1) and best = ref min_int in
        Array.iteri
          (fun i width ->
            let diff = width - median columns.(i).median_values in
            if
              columns.(i).fixed_width = 0
              && ((not floor) || width > min_width i)
              && diff > 0 && diff > !best
            then (
              chosen := i;
              best := diff))
          widths;
        if !chosen < 0 then ()
        else (
          widths.(!chosen) <- widths.(!chosen) - 1;
          loop ())
    in
    loop ()
  in
  decrement ~very_big:true ~floor:true;
  toward_median ~floor:true;
  decrement ~very_big:false ~floor:true;
  if total () > target then begin
    decrement ~very_big:true ~floor:false;
    toward_median ~floor:false;
    decrement ~very_big:false ~floor:false
  end;
  widths

let resize_widths columns ~border_width requested =
  let max_widths =
    Array.map
      (fun c -> if c.fixed_width > 0 then c.fixed_width else c.max_width + c.x_frame)
      columns
  in
  match requested with
  | None -> max_widths
  | Some target when Array.fold_left ( + ) border_width max_widths <= target ->
      let widths = Array.copy max_widths in
      let rec grow () =
        if Array.fold_left ( + ) border_width widths >= target then ()
        else
          let chosen = ref (-1) and shortest = ref max_int in
          Array.iteri
            (fun i width ->
              if columns.(i).fixed_width = 0 && width < !shortest then (
                chosen := i;
                shortest := width))
            widths;
          if !chosen < 0 then ()
          else (
            widths.(!chosen) <- widths.(!chosen) + 1;
            grow ())
      in
      grow ();
      widths
  | Some target -> shrink_widths columns ~border_width target

let style_render ~column_width ~row_height style content =
  let _, _, margin = side_values style in
  let hmargin = margin.Sides.left + margin.Sides.right in
  let vmargin = margin.Sides.top + margin.Sides.bottom in
  let target_width = max 0 (column_width - hmargin) in
  let target_height = max 0 (row_height - vmargin) in
  style |> Style.width target_width |> Style.height target_height
  |> Style.max_width column_width |> Style.max_height row_height
  |> fun s -> Style.render s content

let truncate_lines ~width content =
  normalize_newlines content |> String.split_on_char '\n'
  |> Stdlib.List.map (Charamel_ansi.Text.truncate ~width ~tail:"…")
  |> String.concat "\n"

let fit_cell ~wrap ~is_header ~column_width ~frame_width content =
  let available = max 0 (column_width - frame_width) in
  if is_header then
    let first_line =
      match String.split_on_char '\n' (normalize_newlines content) with
      | [] -> ""
      | first :: _ -> first
    in
    Charamel_ansi.Text.truncate ~width:available ~tail:"…" first_line
  else if not wrap then truncate_lines ~width:available content
  else Charamel_ansi.Text.wrap ~width:available content

type metrics = {
  columns : int;
  widths : int array;
  heights : int array;
  styles : Style.t array array;
  header_row : row;
  data_rows : row list;
  header_count : int;
  data_count : int;
  top : bool;
  bottom : bool;
  header_line : bool;
  row_line : bool;
  first : int;
  last : int;
  overflow : int;
}

let empty_metrics =
  {
    columns = 0;
    widths = [||];
    heights = [||];
    styles = [||];
    header_row = [];
    data_rows = [];
    header_count = 0;
    data_count = 0;
    top = false;
    bottom = false;
    header_line = false;
    row_line = false;
    first = 0;
    last = -1;
    overflow = 0;
  }

let column_metadata t has_headers all_rows columns =
  let meta =
    Array.init columns (fun _ ->
        {
          min_width = max_int;
          max_width = 0;
          x_frame = 0;
          fixed_width = 0;
          median_values = [];
        })
  in
  Stdlib.List.iteri
    (fun row_index row ->
      Stdlib.List.iteri
        (fun col cell ->
          if col < columns then begin
            let style = style_for t has_headers row_index col in
            let width = Layout.width cell in
            let c = meta.(col) in
            c.min_width <- min c.min_width width;
            c.max_width <- max c.max_width width;
            c.x_frame <- max c.x_frame (frame_width style);
            c.fixed_width <- max c.fixed_width (style_get_width style);
            if not (has_headers && row_index = 0) then
              c.median_values <- width :: c.median_values
          end)
        row)
    all_rows;
  Array.iter (fun c -> if c.min_width = max_int then c.min_width <- 0) meta;
  meta

let row_window t heights styles ~header_count ~data_count ~top ~bottom ~header_line =
  match t.height with
  | None -> (min t.offset data_count, data_count - 1, 0)
  | Some table_height ->
      let available =
        ref
          (max 0
             (table_height
             - (if top then 1 else 0)
             - (if bottom then 1 else 0)
             - (if header_count > 0 then heights.(0) else 0)
             - if header_line then 1 else 0))
      in
      let first = ref (min t.offset data_count) in
      let last = ref (!first - 1) in
      while !available > 0 && !last < data_count - 1 do
        let next = !last + 1 in
        let need_overflow =
          if next < data_count - 1 then
            1 + frame_height styles.(header_count + next + 1).(0)
          else 0
        in
        let row_height_value = heights.(header_count + next) in
        if !available - row_height_value - need_overflow < 0 then available := 0
        else begin
          last := next;
          available := !available - row_height_value
        end
      done;
      if !last = data_count - 1 then begin
        while !available > 0 && !first > 0 do
          let previous = !first - 1 in
          let row_height_value = heights.(header_count + previous) in
          if !available - row_height_value < 0 then available := 0
          else begin
            first := previous;
            available := !available - row_height_value
          end
        done;
        (!first, !last, 0)
      end
      else
        let next = !last + 1 in
        ( !first,
          !last,
          1
          + if next < data_count then frame_height styles.(header_count + next).(0) else 0
        )

let metrics t =
  let data_rows = Data.matrix t.data in
  let has_headers = t.headers <> [] in
  let columns =
    max
      (Stdlib.List.length t.headers)
      (Stdlib.List.fold_left (fun n row -> max n (Stdlib.List.length row)) 0 data_rows)
  in
  if columns = 0 then empty_metrics
  else begin
    let header_row = if has_headers then pad_to columns t.headers else [] in
    let all_rows = (if has_headers then [ header_row ] else []) @ data_rows in
    let header_count = if has_headers then 1 else 0 in
    let meta = column_metadata t has_headers all_rows columns in
    let border_width =
      border_budget t.border ~left:t.border_left ~right:t.border_right
        ~column:t.border_column columns
    in
    let content_width =
      Array.fold_left
        (fun sum c ->
          sum + if c.fixed_width > 0 then c.fixed_width else c.max_width + c.x_frame)
        border_width meta
    in
    let requested =
      match (t.width, t.fit_content) with
      | Some width, true when content_width < width -> None
      | requested, _ -> requested
    in
    let widths = resize_widths meta ~border_width requested in
    let width_at i = widths.(i) in
    let styles =
      Array.init (Stdlib.List.length all_rows) (fun row_index ->
          Array.init columns (fun col -> style_for t has_headers row_index col))
    in
    let heights =
      Array.of_list
        (Stdlib.List.mapi
           (fun row_index row ->
             let styles_row = styles.(row_index) in
             let height = ref 1 in
             Stdlib.List.iteri
               (fun col cell ->
                 let style = styles_row.(col) in
                 let frame = frame_width style in
                 let available = max 0 (width_at col - frame) in
                 let content_h =
                   if has_headers && row_index = 0 then 1
                   else if t.wrap then wrapped_height ~width:available cell
                   else line_height cell
                 in
                 height :=
                   max !height
                     (max (style_get_height style) (content_h + frame_height style)))
               row;
             !height)
           all_rows)
    in
    let top =
      t.border_top
      && border_line_present t.border.Border.top_left t.border.top t.border.middle_top
           t.border.top_right
    in
    let bottom =
      t.border_bottom
      && border_line_present t.border.bottom_left t.border.bottom t.border.middle_bottom
           t.border.bottom_right
    in
    let header_line =
      t.border_header && has_headers
      && border_line_present t.border.middle_left t.border.top t.border.middle
           t.border.middle_right
    in
    let row_line =
      t.border_row
      && border_line_present t.border.middle_left t.border.bottom t.border.middle
           t.border.middle_right
    in
    let data_count = Stdlib.List.length data_rows in
    let first, last, overflow =
      row_window t heights styles ~header_count ~data_count ~top ~bottom ~header_line
    in
    {
      columns;
      widths;
      heights;
      styles;
      header_row;
      data_rows;
      header_count;
      data_count;
      top;
      bottom;
      header_line;
      row_line;
      first;
      last;
      overflow;
    }
  end

let first_visible_row t = (metrics t).first

let last_visible_row t =
  let m = metrics t in
  if m.last >= m.data_count - 1 then -1 else m.last

let visible_rows t =
  let m = metrics t in
  match last_visible_row t with
  | -1 -> max 0 (m.data_count - m.first)
  | last -> last - m.first + 1

let body_line t ~left ~separator ~right widths line_index cells =
  let b = Buffer.create 64 in
  let paint = Style.render (border_style t) in
  if left <> "" then Buffer.add_string b (paint left);
  Array.iteri
    (fun col cell_lines ->
      let line = nth_default ~default:"" cell_lines line_index in
      Buffer.add_string b (Charamel_ansi.Text.pad_right ~width:widths.(col) line);
      if col < Array.length widths - 1 && separator <> "" then
        Buffer.add_string b (paint separator))
    cells;
  if right <> "" then Buffer.add_string b (paint right);
  Buffer.contents b

let row_lines t m row_index row is_overflow row_height_value =
  let has_headers = m.header_count > 0 in
  let styles_row =
    if is_overflow then
      Array.init m.columns (fun col ->
          style_for t has_headers (m.header_count + row_index + 1) col)
    else if row_index < 0 then m.styles.(0)
    else m.styles.(m.header_count + row_index)
  in
  let values = if is_overflow then Stdlib.List.init m.columns (fun _ -> "…") else row in
  let cells =
    Array.init m.columns (fun col ->
        let style = styles_row.(col) in
        let frame = frame_width style in
        let value = nth_default ~default:"" values col in
        let fitted =
          fit_cell ~wrap:t.wrap
            ~is_header:(has_headers && row_index < 0)
            ~column_width:m.widths.(col) ~frame_width:frame value
        in
        let rendered =
          style_render ~column_width:m.widths.(col) ~row_height:row_height_value style
            fitted
        in
        String.split_on_char '\n' rendered)
  in
  let left = if t.border_left then t.border.Border.left else "" in
  let right = if t.border_right then t.border.Border.right else "" in
  let separator = if t.border_column then t.border.Border.left else "" in
  Stdlib.List.init (max 1 row_height_value) (fun line_index ->
      body_line t ~left ~separator ~right m.widths line_index cells)

let render t =
  let m = metrics t in
  if m.columns = 0 then ""
  else begin
    let paint = Style.render (border_style t) in
    let result = ref [] in
    let add line = if line <> "" then result := line :: !result in
    let line left segment middle right =
      let left = if t.border_left then left else "" in
      let right = if t.border_right then right else "" in
      let middle = if t.border_column then middle else "" in
      horizontal_line m.widths ~left ~right ~middle ~segment ~paint
    in
    if m.top then
      add
        (line t.border.Border.top_left t.border.top t.border.middle_top t.border.top_right);
    if m.header_count > 0 then begin
      Stdlib.List.iter add (row_lines t m (-1) m.header_row false m.heights.(0));
      if m.header_line then
        add (line t.border.middle_left t.border.top t.border.middle t.border.middle_right)
    end;
    if m.data_count > 0 then begin
      if m.first <= m.last then
        for i = m.first to m.last do
          Stdlib.List.iter add
            (row_lines t m i
               (nth_default ~default:[] m.data_rows i)
               false
               m.heights.(m.header_count + i));
          if m.row_line && i < m.last then
            add
              (line t.border.Border.middle_left t.border.bottom t.border.middle
                 t.border.middle_right)
        done;
      if m.overflow > 0 then
        Stdlib.List.iter add (row_lines t m m.last [] true m.overflow)
    end;
    if m.bottom then
      add
        (line t.border.bottom_left t.border.bottom t.border.middle_bottom
           t.border.bottom_right);
    let lines = Stdlib.List.rev !result in
    match t.height with
    | None -> String.concat "\n" lines
    | Some h -> String.concat "\n" (Stdlib.List.filteri (fun i _ -> i < max 0 h) lines)
  end
