type t = {
  headers : string list;
  rows : string list list;
  border : Border.t;
  style : (row:int -> col:int -> Style.t) option;
  width : int option;
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

let v ?(headers = []) ?(rows = []) ?(border = Border.normal) ?style ?width ?height
    ?(offset = 0) ?(wrap = false) () =
  { headers; rows; border; style; width; height; offset = max 0 offset; wrap }

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

let border_budget (border : Border.t) columns =
  if columns = 0 then 0
  else
    let width s = Charamel_ansi.Text.width s in
    let left =
      max (width border.Border.left)
        (max
           (width border.Border.top_left)
           (max (width border.Border.middle_left) (width border.Border.bottom_left)))
    in
    let right =
      max (width border.Border.right)
        (max
           (width border.Border.top_right)
           (max (width border.Border.middle_right) (width border.Border.bottom_right)))
    in
    let between =
      max (width border.Border.left)
        (max
           (width border.Border.middle_top)
           (max (width border.Border.middle) (width border.Border.middle_bottom)))
    in
    left + right + max 0 ((columns - 1) * between)

let horizontal_line widths ~left ~right ~middle ~segment =
  let b = Buffer.create 64 in
  if left <> "" then Buffer.add_string b left;
  Stdlib.List.iteri
    (fun i width ->
      Buffer.add_string b (repeat_glyph segment width);
      if i < Stdlib.List.length widths - 1 && middle <> "" then Buffer.add_string b middle)
    widths;
  if right <> "" then Buffer.add_string b right;
  Buffer.contents b

let border_line_present a b c d = a <> "" || b <> "" || c <> "" || d <> ""

let style_for t has_headers row col =
  match t.style with
  | None -> Style.empty
  | Some f -> f ~row:(if has_headers then row - 1 else row) ~col

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

let shrink_widths columns border target =
  let border_width = border_budget border (Array.length columns) in
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
  Array.to_list widths

let resize_widths columns border requested =
  let max_widths =
    Array.map
      (fun c -> if c.fixed_width > 0 then c.fixed_width else c.max_width + c.x_frame)
      columns
  in
  let border_width = border_budget border (Array.length columns) in
  match requested with
  | None -> Array.to_list max_widths
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
      Array.to_list widths
  | Some target -> shrink_widths columns border target

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

let render t =
  let has_headers = t.headers <> [] in
  let columns =
    max
      (Stdlib.List.length t.headers)
      (Stdlib.List.fold_left (fun n row -> max n (Stdlib.List.length row)) 0 t.rows)
  in
  if columns = 0 then ""
  else
    let headers = if has_headers then [ pad_to columns t.headers ] else [] in
    let all_rows = headers @ t.rows in
    let header_count = if has_headers then 1 else 0 in
    let columns_meta =
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
              let c = columns_meta.(col) in
              c.min_width <- min c.min_width width;
              c.max_width <- max c.max_width width;
              c.x_frame <- max c.x_frame (frame_width style);
              c.fixed_width <- max c.fixed_width (style_get_width style);
              if not (has_headers && row_index = 0) then
                c.median_values <- width :: c.median_values
            end)
          row)
      all_rows;
    Array.iter (fun c -> if c.min_width = max_int then c.min_width <- 0) columns_meta;
    let widths = resize_widths columns_meta t.border t.width in
    (* One entry per column, so every [col < columns] index is in range. *)
    let column_widths = Array.of_list widths in
    let width_at i = column_widths.(i) in
    let row_styles row_index =
      Array.init columns (fun col -> style_for t has_headers row_index col)
    in
    let styles = Array.init (Stdlib.List.length all_rows) row_styles in
    let row_height row_index row =
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
            max !height (max (style_get_height style) (content_h + frame_height style)))
        row;
      !height
    in
    let heights = Array.of_list (Stdlib.List.mapi row_height all_rows) in
    let top_present =
      border_line_present t.border.Border.top_left t.border.Border.top
        t.border.Border.middle_top t.border.Border.top_right
    in
    let bottom_present =
      border_line_present t.border.Border.bottom_left t.border.Border.bottom
        t.border.Border.middle_bottom t.border.Border.bottom_right
    in
    let header_line_present =
      has_headers
      && border_line_present t.border.Border.middle_left t.border.Border.top
           t.border.Border.middle t.border.Border.middle_right
    in
    let data_count = Stdlib.List.length t.rows in
    let first, last, overflow_height =
      match t.height with
      | None -> (min t.offset data_count, data_count - 1, 0)
      | Some table_height ->
          let available =
            ref
              (max 0
                 (table_height
                 - (if top_present then 1 else 0)
                 - (if bottom_present then 1 else 0)
                 - (if has_headers then heights.(0) else 0)
                 - if header_line_present then 1 else 0))
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
              +
              if next < data_count then frame_height styles.(header_count + next).(0)
              else 0 )
    in
    let render_row row_index row is_overflow row_height_value =
      let styles_row =
        if is_overflow then
          Array.init columns (fun col ->
              style_for t has_headers (header_count + row_index + 1) col)
        else if row_index < 0 then styles.(0)
        else styles.(header_count + row_index)
      in
      let values = if is_overflow then Stdlib.List.init columns (fun _ -> "…") else row in
      let lines =
        Array.init columns (fun col ->
            let style = styles_row.(col) in
            let frame = frame_width style in
            let value = nth_default ~default:"" values col in
            let fitted =
              fit_cell ~wrap:t.wrap
                ~is_header:(has_headers && row_index = -1)
                ~column_width:(width_at col) ~frame_width:frame value
            in
            let rendered =
              style_render ~column_width:(width_at col) ~row_height:row_height_value style
                fitted
            in
            String.split_on_char '\n' rendered)
      in
      let line_count = max 1 row_height_value in
      Stdlib.List.init line_count (fun line_index ->
          let b = Buffer.create 64 in
          if t.border.Border.left <> "" then Buffer.add_string b t.border.Border.left;
          Array.iteri
            (fun col cell_lines ->
              let line = nth_default ~default:"" cell_lines line_index in
              Buffer.add_string b
                (Charamel_ansi.Text.pad_right ~width:(width_at col) line);
              if col < columns - 1 && t.border.Border.left <> "" then
                Buffer.add_string b t.border.Border.left)
            lines;
          if t.border.Border.right <> "" then Buffer.add_string b t.border.Border.right;
          Buffer.contents b)
    in
    let result = ref [] in
    let add line = if line <> "" then result := line :: !result in
    if top_present then
      add
        (horizontal_line widths ~left:t.border.Border.top_left
           ~right:t.border.Border.top_right ~middle:t.border.Border.middle_top
           ~segment:t.border.Border.top);
    if has_headers then begin
      Stdlib.List.iter add
        (render_row (-1) (nth_default ~default:[] all_rows 0) false heights.(0));
      if header_line_present then
        add
          (horizontal_line widths ~left:t.border.Border.middle_left
             ~right:t.border.Border.middle_right ~middle:t.border.Border.middle
             ~segment:t.border.Border.top)
    end;
    if data_count > 0 then begin
      if first <= last then
        for i = first to last do
          Stdlib.List.iter add
            (render_row i
               (nth_default ~default:[] t.rows i)
               false
               heights.(header_count + i))
        done;
      if overflow_height > 0 then
        Stdlib.List.iter add (render_row last [] true overflow_height)
    end;
    if bottom_present then
      add
        (horizontal_line widths ~left:t.border.Border.bottom_left
           ~right:t.border.Border.bottom_right ~middle:t.border.Border.middle_bottom
           ~segment:t.border.Border.bottom);
    let lines = Stdlib.List.rev !result in
    let lines =
      match t.height with
      | None -> lines
      | Some h -> Stdlib.List.filteri (fun i _ -> i < max 0 h) lines
    in
    String.concat "\n" lines
