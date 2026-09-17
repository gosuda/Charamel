type cell = {
  text : string;
  width : int;
  style : Charm_ansi.Style.t;
  link : Charm_ansi.Link.t option;
  cont : bool;
}

let link_equal a b =
  match (a, b) with
  | None, None -> true
  | Some (x : Charm_ansi.Link.t), Some (y : Charm_ansi.Link.t) ->
      String.equal x.Charm_ansi.Link.url y.Charm_ansi.Link.url
      && x.Charm_ansi.Link.params = y.Charm_ansi.Link.params
  | _ -> false

let cell_equal a b =
  a.width = b.width && a.cont = b.cont && String.equal a.text b.text
  && Charm_ansi.Style.equal a.style b.style
  && link_equal a.link b.link

let blank_cell =
  { text = " "; width = 1; style = Charm_ansi.Style.default; link = None; cont = false }

let is_blank c = cell_equal c blank_cell

let last_nonblank line =
  let rec loop i = if i < 0 then -1 else if is_blank line.(i) then loop (i - 1) else i in
  loop (Array.length line - 1)

let first_diff old_line new_line n =
  let rec loop i =
    if i >= n then None
    else if i < Array.length old_line && cell_equal old_line.(i) new_line.(i) then
      loop (i + 1)
    else Some i
  in
  loop 0

let line_bounds ~force old_line new_line =
  let n = Array.length new_line in
  if n = 0 then if force || last_nonblank old_line >= 0 then Some (0, -1, true) else None
  else
    Option.map
      (fun first ->
        let first = if first > 0 && new_line.(first).cont then first - 1 else first in
        let old_last = last_nonblank old_line in
        let new_last = last_nonblank new_line in
        let clear_tail = force || old_last > new_last in
        let last = max first new_last in
        let last = if last < n - 1 && new_line.(last).cont then last + 1 else last in
        (first, last, clear_tail))
      (if force then Some 0 else first_diff old_line new_line n)

let color_value = function Some n -> n | None -> 0

let underline_of = function
  | [] | [ None ] | [ Some 1 ] -> Charm_ansi.Style.Single
  | [ Some 0 ] -> Charm_ansi.Style.No_underline
  | [ Some 2 ] -> Charm_ansi.Style.Double
  | [ Some 3 ] -> Charm_ansi.Style.Curly
  | [ Some 4 ] -> Charm_ansi.Style.Dotted
  | [ Some 5 ] -> Charm_ansi.Style.Dashed
  | _ -> Charm_ansi.Style.Single

let extended_color subs rest =
  let values, rest =
    if subs <> [] then (subs, rest)
    else
      match rest with
      | [ Some 5 ] :: index :: more -> ([ Some 5 ] @ index, more)
      | [ Some 2 ] :: r :: g :: b :: more -> ([ Some 2 ] @ r @ g @ b, more)
      | _ -> ([], rest)
  in
  let color =
    match values with
    | Some 5 :: index :: _ -> (
        match Charm_ansi.Color.indexed (color_value index) with
        | Some color -> color
        | None -> Charm_ansi.Color.Default)
    | Some 2 :: _colorspace :: r :: g :: b :: _ -> (
        match Charm_ansi.Color.rgb (color_value r) (color_value g) (color_value b) with
        | Some color -> color
        | None -> Charm_ansi.Color.Default)
    | Some 2 :: r :: g :: b :: _ -> (
        match Charm_ansi.Color.rgb (color_value r) (color_value g) (color_value b) with
        | Some color -> color
        | None -> Charm_ansi.Color.Default)
    | _ -> Charm_ansi.Color.Default
  in
  (color, rest)

let rec apply_sgr (style : Charm_ansi.Style.t) params =
  match params with
  | [] -> style
  | parameter :: rest -> (
      match parameter with
      | [] | [ None ] | [ Some 0 ] -> apply_sgr Charm_ansi.Style.default rest
      | [ Some 1 ] -> apply_sgr { style with bold = true } rest
      | [ Some 2 ] -> apply_sgr { style with faint = true } rest
      | [ Some 3 ] -> apply_sgr { style with italic = true } rest
      | Some 4 :: subparameters ->
          apply_sgr { style with underline = underline_of subparameters } rest
      | [ Some 5 ] | [ Some 6 ] -> apply_sgr { style with blink = true } rest
      | [ Some 7 ] -> apply_sgr { style with reverse = true } rest
      | [ Some 8 ] -> apply_sgr { style with conceal = true } rest
      | [ Some 9 ] -> apply_sgr { style with strike = true } rest
      | [ Some 22 ] -> apply_sgr { style with bold = false; faint = false } rest
      | [ Some 23 ] -> apply_sgr { style with italic = false } rest
      | [ Some 24 ] ->
          apply_sgr { style with underline = Charm_ansi.Style.No_underline } rest
      | [ Some 25 ] -> apply_sgr { style with blink = false } rest
      | [ Some 27 ] -> apply_sgr { style with reverse = false } rest
      | [ Some 28 ] -> apply_sgr { style with conceal = false } rest
      | [ Some 29 ] -> apply_sgr { style with strike = false } rest
      | Some 38 :: subparameters ->
          let color, rest = extended_color subparameters rest in
          apply_sgr { style with fg = color } rest
      | [ Some 39 ] -> apply_sgr { style with fg = Charm_ansi.Color.Default } rest
      | Some 48 :: subparameters ->
          let color, rest = extended_color subparameters rest in
          apply_sgr { style with bg = color } rest
      | [ Some 49 ] -> apply_sgr { style with bg = Charm_ansi.Color.Default } rest
      | Some 58 :: subparameters ->
          let color, rest = extended_color subparameters rest in
          apply_sgr { style with underline_color = color } rest
      | [ Some 59 ] ->
          apply_sgr { style with underline_color = Charm_ansi.Color.Default } rest
      | [ Some n ] when n >= 30 && n <= 37 ->
          apply_sgr { style with fg = Charm_ansi.Color.Basic (n - 30) } rest
      | [ Some n ] when n >= 40 && n <= 47 ->
          apply_sgr { style with bg = Charm_ansi.Color.Basic (n - 40) } rest
      | [ Some n ] when n >= 90 && n <= 97 ->
          apply_sgr { style with fg = Charm_ansi.Color.Basic (n - 90 + 8) } rest
      | [ Some n ] when n >= 100 && n <= 107 ->
          apply_sgr { style with bg = Charm_ansi.Color.Basic (n - 100 + 8) } rest
      | _ -> apply_sgr style rest)

let valid_parameter s =
  let len = String.length s in
  let rec loop i =
    if i >= len then true
    else
      let decoded = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid decoded) then false
      else
        let scalar = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
        scalar <> 0x07 && scalar <> 0x1b
        && (scalar < 0x80 || scalar > 0x9f)
        && loop (i + Uchar.utf_decode_length decoded)
  in
  loop 0

let parse_link_params s =
  if s = "" then []
  else
    String.split_on_char ':' s
    |> List.filter_map (fun field ->
        Option.bind (String.index_opt field '=') (fun index ->
            let key = String.sub field 0 index in
            let value = String.sub field (index + 1) (String.length field - index - 1) in
            if
              key <> ""
              && (not (String.contains key ';'))
              && (not (String.contains key ':'))
              && (not (String.contains value ':'))
              && (not (String.contains value ';'))
              && valid_parameter key && valid_parameter value
            then Some (key, value)
            else None))

let layout ~cols ~max_rows content =
  if cols <= 0 || max_rows <= 0 then [||]
  else begin
    let rows = Dynarray.create () in
    let current = ref (Array.make cols blank_cell) in
    let column = ref 0 in
    let style = ref Charm_ansi.Style.default in
    let link = ref None in
    let full = ref false in
    let finish_line () =
      if not !full then begin
        Dynarray.add_last rows !current;
        if Dynarray.length rows >= max_rows then full := true
      end;
      current := Array.make cols blank_cell;
      column := 0
    in
    let append_zero_width text =
      if !column > 0 then begin
        let index = ref (!column - 1) in
        if !current.(!index).cont then decr index;
        let previous = !current.(!index) in
        !current.(!index) <- { previous with text = previous.text ^ text }
      end
    in
    let clear_overlap width =
      if !column < cols && !current.(!column).cont then begin
        !current.(!column) <- blank_cell;
        !current.(!column - 1) <- blank_cell
      end
      else if width = 1 && !column + 1 < cols && !current.(!column + 1).cont then
        !current.(!column + 1) <- blank_cell
    in
    let rec put text width =
      if !full then ()
      else if width = 0 then append_zero_width text
      else if width > cols then ()
      else if !column + width > cols then begin
        finish_line ();
        put text width
      end
      else begin
        clear_overlap width;
        let cell = { text; width; style = !style; link = !link; cont = false } in
        !current.(!column) <- cell;
        if width = 2 then !current.(!column + 1) <- { cell with text = ""; cont = true };
        column := !column + width
      end
    in
    let tab () = column := min cols (((!column / 8) + 1) * 8) in
    let pending = Buffer.create 16 in
    let flush_print () =
      if Buffer.length pending > 0 then begin
        let text = Buffer.contents pending in
        Buffer.clear pending;
        List.iter
          (fun grapheme -> put grapheme (Charm_ansi.Width.grapheme_width grapheme))
          (Charm_ansi.Width.graphemes text)
      end
    in
    let handle action =
      match action with
      | Charm_ansi.Parser.Print text -> Buffer.add_string pending text
      | Charm_ansi.Parser.Execute '\n' ->
          flush_print ();
          finish_line ()
      | Charm_ansi.Parser.Execute '\r' ->
          flush_print ();
          column := 0
      | Charm_ansi.Parser.Execute '\t' ->
          flush_print ();
          tab ()
      | Charm_ansi.Parser.Execute '\b' ->
          flush_print ();
          column := max 0 (!column - 1)
      | Charm_ansi.Parser.Execute _ -> flush_print ()
      | Charm_ansi.Parser.Csi { final = 'm'; params = []; _ } ->
          flush_print ();
          style := apply_sgr !style [ [ Some 0 ] ]
      | Charm_ansi.Parser.Csi { final = 'm'; params; _ } ->
          flush_print ();
          style := apply_sgr !style params
      | Charm_ansi.Parser.Csi _ -> flush_print ()
      | Charm_ansi.Parser.Osc ("8" :: parameters :: rest) ->
          flush_print ();
          let uri = String.concat ";" rest in
          link :=
            if uri = "" || not (valid_parameter uri) then None
            else Some { Charm_ansi.Link.url = uri; params = parse_link_params parameters }
      | Charm_ansi.Parser.Osc _ -> flush_print ()
      | Charm_ansi.Parser.Esc _ | Charm_ansi.Parser.Dcs _ | Charm_ansi.Parser.Apc _
      | Charm_ansi.Parser.Pm _ | Charm_ansi.Parser.Sos _ ->
          flush_print ()
    in
    let parser = Charm_ansi.Parser.create () in
    List.iter handle (Charm_ansi.Parser.feed parser content);
    List.iter handle (Charm_ansi.Parser.flush parser);
    flush_print ();
    finish_line ();
    Dynarray.to_array rows
  end

let pad_rows grid rows cols =
  if rows <= 0 then [||]
  else
    let have = Array.length grid in
    if have >= rows then Array.sub grid 0 rows
    else
      Array.append grid (Array.init (rows - have) (fun _ -> Array.make cols blank_cell))

let mouse_modes = function
  | View.Mouse_off -> []
  | View.Mouse_click -> [ Charm_ansi.Seq.mouse_click; Charm_ansi.Seq.mouse_sgr ]
  | View.Mouse_motion -> [ Charm_ansi.Seq.mouse_motion; Charm_ansi.Seq.mouse_sgr ]
  | View.Mouse_all -> [ Charm_ansi.Seq.mouse_all; Charm_ansi.Seq.mouse_sgr ]

let emit_mouse_change buf old_mode new_mode =
  let old_modes = mouse_modes old_mode and new_modes = mouse_modes new_mode in
  List.iter
    (fun mode ->
      if not (List.mem mode new_modes) then
        Buffer.add_string buf (Charm_ansi.Seq.decrst mode))
    old_modes;
  List.iter
    (fun mode ->
      if not (List.mem mode old_modes) then
        Buffer.add_string buf (Charm_ansi.Seq.decset mode))
    new_modes

let kitty_flags (keyboard : View.keyboard) =
  (if keyboard.View.disambiguate then 1 else 0)
  lor (if keyboard.View.report_events then 2 else 0)
  lor (if keyboard.View.report_alternates then 4 else 0)
  lor (if keyboard.View.report_all_keys then 8 else 0)
  lor if keyboard.View.report_text then 16 else 0

let cursor_style_code (cursor : Cursor.t) =
  let shape =
    match cursor.Cursor.shape with
    | Cursor.Block -> 0
    | Cursor.Underline -> 1
    | Cursor.Bar -> 2
  in
  (shape * 2) + if cursor.Cursor.blink then 1 else 2

let clamp_byte value = max 0 (min 255 value)

let hex_of_color = function
  | Charm_ansi.Color.Rgb (r, g, b) ->
      Some (Fmt.str "#%02x%02x%02x" (clamp_byte r) (clamp_byte g) (clamp_byte b))
  | _ -> None

let applied_color = function Some (Charm_ansi.Color.Rgb _) as color -> color | _ -> None
let clamp_pct n = max 0 (min 100 n)

let progress_osc = function
  | View.Progress_none -> "\x1b]9;4;0\x07"
  | View.Progress_value n -> Fmt.str "\x1b]9;4;1;%d\x07" (clamp_pct n)
  | View.Progress_error n -> Fmt.str "\x1b]9;4;2;%d\x07" (clamp_pct n)
  | View.Progress_indeterminate -> "\x1b]9;4;3\x07"
  | View.Progress_warning n -> Fmt.str "\x1b]9;4;4;%d\x07" (clamp_pct n)

type t = {
  mutable rows : int;
  mutable cols : int;
  mutable alt : bool;
  mutable alt_grid : cell array array;
  mutable alt_needs_clear : bool;
  mutable alt_pos : (int * int) option;
  mutable inline_grid : cell array array;
  mutable h_prev : int;
  mutable inline_poison : bool;
  mutable inline_pos : int * int;
  mutable emitted_style : Charm_ansi.Style.t option;
  mutable link_open : Charm_ansi.Link.t option;
  mutable mouse : View.mouse_mode;
  mutable paste : bool;
  mutable focus : bool;
  mutable cursor_visible : bool;
  mutable cursor_shape_code : int;
  mutable title : string option;
  mutable bg : Charm_ansi.Color.t option;
  mutable fg : Charm_ansi.Color.t option;
  mutable progress : View.progress;
  mutable inline_pushed : bool;
  mutable inline_kflags : int;
  mutable alt_pushed : bool;
  mutable alt_kflags : int;
}

let create ~rows ~cols =
  {
    rows;
    cols;
    alt = false;
    alt_grid = [||];
    alt_needs_clear = false;
    alt_pos = None;
    inline_grid = [||];
    h_prev = 0;
    inline_poison = true;
    inline_pos = (0, 0);
    emitted_style = Some Charm_ansi.Style.default;
    link_open = None;
    mouse = View.Mouse_off;
    paste = false;
    focus = false;
    cursor_visible = true;
    cursor_shape_code = 1;
    title = None;
    bg = None;
    fg = None;
    progress = View.Progress_none;
    inline_pushed = false;
    inline_kflags = 0;
    alt_pushed = false;
    alt_kflags = 0;
  }

let resize t ~rows ~cols =
  t.rows <- rows;
  t.cols <- cols;
  if t.alt then begin
    t.alt_needs_clear <- true;
    t.alt_pos <- Some (0, 0)
  end
  else t.inline_pos <- (0, 0);
  t.inline_poison <- true

let reset t =
  t.alt <- false;
  t.alt_grid <- [||];
  t.alt_needs_clear <- false;
  t.alt_pos <- None;
  t.inline_grid <- [||];
  t.h_prev <- 0;
  t.inline_poison <- true;
  t.inline_pos <- (0, 0);
  t.emitted_style <- Some Charm_ansi.Style.default;
  t.link_open <- None;
  t.mouse <- View.Mouse_off;
  t.paste <- false;
  t.focus <- false;
  t.cursor_visible <- true;
  t.cursor_shape_code <- 1;
  t.title <- None;
  t.bg <- None;
  t.fg <- None;
  t.progress <- View.Progress_none;
  t.inline_pushed <- false;
  t.inline_kflags <- 0;
  t.alt_pushed <- false;
  t.alt_kflags <- 0

let reset_pen buf t =
  (match t.link_open with
  | Some _ -> Buffer.add_string buf (Charm_ansi.Link.osc8 None)
  | None -> ());
  (match t.emitted_style with
  | Some style when Charm_ansi.Style.equal style Charm_ansi.Style.default -> ()
  | _ -> Buffer.add_string buf (Charm_ansi.Style.to_sgr Charm_ansi.Style.default));
  t.link_open <- None;
  t.emitted_style <- Some Charm_ansi.Style.default

let write_run buf emitted_style link_open new_line lo hi =
  for index = lo to hi do
    if not new_line.(index).cont then begin
      let cell = new_line.(index) in
      if not (link_equal cell.link !link_open) then begin
        Buffer.add_string buf (Charm_ansi.Link.osc8 cell.link);
        link_open := cell.link
      end;
      (match !emitted_style with
      | None -> Buffer.add_string buf (Charm_ansi.Style.to_sgr cell.style)
      | Some style ->
          if not (Charm_ansi.Style.equal style cell.style) then
            Buffer.add_string buf (Charm_ansi.Style.transition ~from:style cell.style));
      emitted_style := Some cell.style;
      Buffer.add_string buf cell.text
    end
  done

let render_alt t buf (view : View.t) =
  if t.alt_needs_clear then begin
    reset_pen buf t;
    Buffer.add_string buf (Charm_ansi.Seq.ed `All);
    Buffer.add_string buf (Charm_ansi.Seq.cup ~row:1 ~col:1);
    t.alt_grid <- Array.init t.rows (fun _ -> Array.make t.cols blank_cell);
    t.alt_needs_clear <- false;
    t.alt_pos <- Some (0, 0)
  end;
  let new_grid =
    pad_rows (layout ~cols:t.cols ~max_rows:t.rows view.View.content) t.rows t.cols
  in
  let emitted_style = ref t.emitted_style and link_open = ref t.link_open in
  let goto row col =
    if t.alt_pos <> Some (row, col) then begin
      Buffer.add_string buf (Charm_ansi.Seq.cup ~row:(row + 1) ~col:(col + 1));
      t.alt_pos <- Some (row, col)
    end
  in
  if Array.length new_grid > 0 then begin
    for row = 0 to min (t.rows - 1) (Array.length new_grid - 1) do
      let old_line =
        if row < Array.length t.alt_grid then t.alt_grid.(row)
        else Array.make t.cols blank_cell
      in
      match line_bounds ~force:false old_line new_grid.(row) with
      | None -> ()
      | Some (lo, hi, clear_tail) ->
          goto row lo;
          let erased_from = last_nonblank new_grid.(row) < lo in
          if erased_from then
            begin match !link_open with
            | Some _ ->
                Buffer.add_string buf (Charm_ansi.Link.osc8 None);
                link_open := None
            | None -> ()
            end
          else write_run buf emitted_style link_open new_grid.(row) lo hi;
          if clear_tail then Buffer.add_string buf (Charm_ansi.Seq.el `To_end);
          t.alt_pos <- Some (row, if erased_from then lo else hi + 1)
    done
  end;
  t.alt_grid <- new_grid;
  t.emitted_style <- !emitted_style;
  t.link_open <- !link_open;
  match view.View.cursor with
  | None -> ()
  | Some cursor ->
      let row = if t.rows <= 0 then 0 else max 0 (min cursor.Cursor.row (t.rows - 1)) in
      let col = if t.cols <= 0 then 0 else max 0 (min cursor.Cursor.col (t.cols - 1)) in
      if t.alt_pos <> Some (row, col) then begin
        Buffer.add_string buf (Charm_ansi.Seq.cup ~row:(row + 1) ~col:(col + 1));
        t.alt_pos <- Some (row, col)
      end

let move_inline buf ~from_row ~from_col ~to_row ~to_col =
  let row = ref from_row and col = ref from_col in
  if to_row > !row then begin
    Buffer.add_char buf '\r';
    for _ = 1 to to_row - !row do
      Buffer.add_char buf '\n'
    done;
    row := to_row;
    col := 0
  end
  else if to_row < !row then begin
    Buffer.add_string buf (Charm_ansi.Seq.cuu (!row - to_row));
    row := to_row
  end;
  if to_col > !col then Buffer.add_string buf (Charm_ansi.Seq.cuf (to_col - !col))
  else if to_col < !col then Buffer.add_string buf (Charm_ansi.Seq.cub (!col - to_col))

let render_inline t buf (view : View.t) =
  let new_window = layout ~cols:t.cols ~max_rows:t.rows view.View.content in
  let h_new = Array.length new_window in
  let h_prev = t.h_prev in
  let old_window = t.inline_grid in
  let emitted_style = ref t.emitted_style and link_open = ref t.link_open in
  let start_row, start_col =
    if t.inline_poison && t.h_prev = 0 then (0, 0) else t.inline_pos
  in
  let current_row = ref start_row and current_col = ref start_col in
  let goto row col =
    move_inline buf ~from_row:!current_row ~from_col:!current_col ~to_row:row ~to_col:col;
    current_row := row;
    current_col := col
  in
  for row = 0 to max h_new h_prev - 1 do
    let old_line =
      if row < h_prev && row < Array.length old_window then old_window.(row)
      else Array.make t.cols blank_cell
    in
    let new_line =
      if row < h_new then new_window.(row) else Array.make t.cols blank_cell
    in
    match line_bounds ~force:t.inline_poison old_line new_line with
    | None -> ()
    | Some (lo, hi, clear_tail) ->
        goto row lo;
        let erased_from = last_nonblank new_line < lo in
        if erased_from then
          begin match !link_open with
          | Some _ ->
              Buffer.add_string buf (Charm_ansi.Link.osc8 None);
              link_open := None
          | None -> ()
          end
        else write_run buf emitted_style link_open new_line lo hi;
        if clear_tail then Buffer.add_string buf (Charm_ansi.Seq.el `To_end);
        current_row := row;
        current_col := if erased_from then lo else hi + 1
  done;
  (match view.View.cursor with
  | Some cursor ->
      let row = if h_new <= 0 then 0 else max 0 (min cursor.Cursor.row (h_new - 1)) in
      let col = if t.cols <= 0 then 0 else max 0 (min cursor.Cursor.col (t.cols - 1)) in
      goto row col
  | None ->
      if h_new > 0 then begin
        let last = last_nonblank new_window.(h_new - 1) in
        let col = last + 1 in
        goto (h_new - 1) col;
        if col >= t.cols && t.cols > 0 then begin
          Buffer.add_char buf '\r';
          current_col := 0
        end
      end
      else goto 0 0);
  t.h_prev <- h_new;
  t.inline_grid <- new_window;
  t.inline_poison <- false;
  t.inline_pos <- (!current_row, !current_col);
  t.emitted_style <- !emitted_style;
  t.link_open <- !link_open

let apply_kitty buf t target =
  let pushed, flags =
    if t.alt then (t.alt_pushed, t.alt_kflags) else (t.inline_pushed, t.inline_kflags)
  in
  if target = 0 then
    begin if pushed then begin
      Buffer.add_string buf Charm_ansi.Seq.kitty_pop;
      if t.alt then begin
        t.alt_pushed <- false;
        t.alt_kflags <- 0
      end
      else begin
        t.inline_pushed <- false;
        t.inline_kflags <- 0
      end
    end
    end
  else if not pushed then begin
    Buffer.add_string buf (Charm_ansi.Seq.kitty_push target);
    if t.alt then begin
      t.alt_pushed <- true;
      t.alt_kflags <- target
    end
    else begin
      t.inline_pushed <- true;
      t.inline_kflags <- target
    end
  end
  else if flags <> target then begin
    Buffer.add_string buf (Fmt.str "\x1b[=%d;1u" target);
    if t.alt then t.alt_kflags <- target else t.inline_kflags <- target
  end

let cursor_visibility_and_shape buf t (cursor : Cursor.t option) =
  let visible = cursor <> None in
  if visible <> t.cursor_visible then begin
    Buffer.add_string buf
      ((if visible then Charm_ansi.Seq.decset else Charm_ansi.Seq.decrst)
         Charm_ansi.Seq.cursor_visible);
    t.cursor_visible <- visible
  end;
  match cursor with
  | None -> ()
  | Some cursor ->
      let code = cursor_style_code cursor in
      if code <> t.cursor_shape_code then begin
        Buffer.add_string buf (if code = 1 then "\x1b[0 q" else Fmt.str "\x1b[%d q" code);
        t.cursor_shape_code <- code
      end

let render t (view : View.t) =
  let buf = Buffer.create 256 in
  let entering_alt = view.View.alt_screen && not t.alt in
  let leaving_alt = (not view.View.alt_screen) && t.alt in
  let target_kflags = kitty_flags view.View.keyboard in
  if leaving_alt then begin
    if t.alt_pushed then begin
      Buffer.add_string buf Charm_ansi.Seq.kitty_pop;
      t.alt_pushed <- false;
      t.alt_kflags <- 0
    end;
    Buffer.add_string buf (Charm_ansi.Seq.decrst Charm_ansi.Seq.alt_screen);
    t.alt <- false;
    apply_kitty buf t target_kflags;
    reset_pen buf t
  end
  else if entering_alt then begin
    Buffer.add_string buf (Charm_ansi.Seq.decset Charm_ansi.Seq.alt_screen);
    t.alt <- true;
    t.alt_grid <- Array.init t.rows (fun _ -> Array.make t.cols blank_cell);
    t.alt_needs_clear <- true;
    t.alt_pos <- Some (0, 0);
    apply_kitty buf t target_kflags
  end
  else apply_kitty buf t target_kflags;
  if view.View.mouse <> t.mouse then begin
    emit_mouse_change buf t.mouse view.View.mouse;
    t.mouse <- view.View.mouse
  end;
  if view.View.bracketed_paste <> t.paste then begin
    Buffer.add_string buf
      ((if view.View.bracketed_paste then Charm_ansi.Seq.decset else Charm_ansi.Seq.decrst)
         Charm_ansi.Seq.bracketed_paste);
    t.paste <- view.View.bracketed_paste
  end;
  if view.View.report_focus <> t.focus then begin
    Buffer.add_string buf
      ((if view.View.report_focus then Charm_ansi.Seq.decset else Charm_ansi.Seq.decrst)
         Charm_ansi.Seq.focus);
    t.focus <- view.View.report_focus
  end;
  (match (view.View.title, t.title) with
  | Some title, previous when previous <> Some title ->
      Buffer.add_string buf (Charm_ansi.Seq.title title);
      t.title <- Some title
  | None, Some _ ->
      Buffer.add_string buf (Charm_ansi.Seq.title "");
      t.title <- None
  | _ -> ());
  let target_bg = applied_color view.View.background
  and target_fg = applied_color view.View.foreground in
  (match (target_bg, t.bg) with
  | Some color, previous
    when not (Option.equal Charm_ansi.Color.equal (Some color) previous) ->
      (match hex_of_color color with
      | Some hex -> Buffer.add_string buf (Fmt.str "\x1b]11;%s\x07" hex)
      | None -> ());
      t.bg <- target_bg
  | None, Some _ ->
      Buffer.add_string buf "\x1b]111\x07";
      t.bg <- None
  | _ -> ());
  (match (target_fg, t.fg) with
  | Some color, previous
    when not (Option.equal Charm_ansi.Color.equal (Some color) previous) ->
      (match hex_of_color color with
      | Some hex -> Buffer.add_string buf (Fmt.str "\x1b]10;%s\x07" hex)
      | None -> ());
      t.fg <- target_fg
  | None, Some _ ->
      Buffer.add_string buf "\x1b]110\x07";
      t.fg <- None
  | _ -> ());
  if view.View.progress <> t.progress then begin
    Buffer.add_string buf (progress_osc view.View.progress);
    t.progress <- view.View.progress
  end;
  if view.View.alt_screen then render_alt t buf view else render_inline t buf view;
  cursor_visibility_and_shape buf t view.View.cursor;
  if Buffer.length buf = 0 then ""
  else
    Charm_ansi.Seq.decset Charm_ansi.Seq.sync_output
    ^ Buffer.contents buf
    ^ Charm_ansi.Seq.decrst Charm_ansi.Seq.sync_output

let clear t =
  if t.alt || t.h_prev = 0 then ""
  else begin
    let buf = Buffer.create 64 in
    let current_row, current_col = t.inline_pos in
    move_inline buf ~from_row:current_row ~from_col:current_col ~to_row:0 ~to_col:0;
    for row = 0 to t.h_prev - 1 do
      Buffer.add_string buf (Charm_ansi.Seq.el `All);
      if row < t.h_prev - 1 then Buffer.add_char buf '\n'
    done;
    if t.h_prev > 1 then Buffer.add_string buf (Charm_ansi.Seq.cuu (t.h_prev - 1));
    Buffer.add_char buf '\r';
    reset_pen buf t;
    t.h_prev <- 0;
    t.inline_grid <- [||];
    t.inline_poison <- true;
    t.inline_pos <- (0, 0);
    Buffer.contents buf
  end

let restore t =
  let buf = Buffer.create 96 in
  Buffer.add_string buf (Charm_ansi.Seq.decrst Charm_ansi.Seq.sync_output);
  if t.alt then begin
    if t.alt_pushed then Buffer.add_string buf Charm_ansi.Seq.kitty_pop;
    Buffer.add_string buf (Charm_ansi.Seq.decrst Charm_ansi.Seq.alt_screen)
  end;
  List.iter
    (fun mode -> Buffer.add_string buf (Charm_ansi.Seq.decrst mode))
    (mouse_modes t.mouse);
  if t.paste then
    Buffer.add_string buf (Charm_ansi.Seq.decrst Charm_ansi.Seq.bracketed_paste);
  if t.focus then Buffer.add_string buf (Charm_ansi.Seq.decrst Charm_ansi.Seq.focus);
  if not t.cursor_visible then
    Buffer.add_string buf (Charm_ansi.Seq.decset Charm_ansi.Seq.cursor_visible);
  if t.cursor_shape_code <> 1 then Buffer.add_string buf "\x1b[0 q";
  (match t.title with
  | Some _ -> Buffer.add_string buf (Charm_ansi.Seq.title "")
  | None -> ());
  (match t.bg with Some _ -> Buffer.add_string buf "\x1b]111\x07" | None -> ());
  (match t.fg with Some _ -> Buffer.add_string buf "\x1b]110\x07" | None -> ());
  if t.progress <> View.Progress_none then
    Buffer.add_string buf (progress_osc View.Progress_none);
  (match t.link_open with
  | Some _ -> Buffer.add_string buf (Charm_ansi.Link.osc8 None)
  | None -> ());
  Buffer.add_string buf (Charm_ansi.Style.to_sgr Charm_ansi.Style.default);
  if t.inline_pushed then Buffer.add_string buf Charm_ansi.Seq.kitty_pop;
  reset t;
  Buffer.contents buf
