open Charamel_ansi.Raster

let first_diff old_line new_line n =
  let rec loop i =
    if i >= n then None
    else if i < Array.length old_line && equal old_line.(i) new_line.(i) then loop (i + 1)
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

let mouse_modes = function
  | View.Mouse_off -> []
  | View.Mouse_click -> [ Charamel_ansi.Seq.mouse_click; Charamel_ansi.Seq.mouse_sgr ]
  | View.Mouse_motion -> [ Charamel_ansi.Seq.mouse_motion; Charamel_ansi.Seq.mouse_sgr ]
  | View.Mouse_all -> [ Charamel_ansi.Seq.mouse_all; Charamel_ansi.Seq.mouse_sgr ]

let emit_mouse_change buf old_mode new_mode =
  let old_modes = mouse_modes old_mode and new_modes = mouse_modes new_mode in
  List.iter
    (fun mode ->
      if not (List.mem mode new_modes) then
        Buffer.add_string buf (Charamel_ansi.Seq.decrst mode))
    old_modes;
  List.iter
    (fun mode ->
      if not (List.mem mode old_modes) then
        Buffer.add_string buf (Charamel_ansi.Seq.decset mode))
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
  | Charamel_ansi.Color.Rgb (r, g, b) ->
      Some (Fmt.str "#%02x%02x%02x" (clamp_byte r) (clamp_byte g) (clamp_byte b))
  | _ -> None

let applied_color = function
  | Some (Charamel_ansi.Color.Rgb _) as color -> color
  | _ -> None

let snap_off_continuation grid row col =
  if row < 0 || row >= Array.length grid then col
  else
    let line = grid.(row) in
    if col > 0 && col < Array.length line && line.(col).cont then col - 1 else col

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
  mutable emitted_style : Charamel_ansi.Style.t option;
  mutable link_open : Charamel_ansi.Link.t option;
  mutable mouse : View.mouse_mode;
  mutable paste : bool;
  mutable focus : bool;
  mutable cursor_visible : bool;
  mutable cursor_shape_code : int;
  mutable cursor_color : Charamel_ansi.Color.t option;
  mutable title : string option;
  mutable bg : Charamel_ansi.Color.t option;
  mutable fg : Charamel_ansi.Color.t option;
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
    emitted_style = Some Charamel_ansi.Style.default;
    link_open = None;
    mouse = View.Mouse_off;
    paste = false;
    focus = false;
    cursor_visible = true;
    cursor_shape_code = 1;
    cursor_color = None;
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
  t.emitted_style <- Some Charamel_ansi.Style.default;
  t.link_open <- None;
  t.mouse <- View.Mouse_off;
  t.paste <- false;
  t.focus <- false;
  t.cursor_visible <- true;
  t.cursor_shape_code <- 1;
  t.cursor_color <- None;
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
  | Some _ -> Buffer.add_string buf (Charamel_ansi.Link.osc8 None)
  | None -> ());
  (match t.emitted_style with
  | Some style when Charamel_ansi.Style.equal style Charamel_ansi.Style.default -> ()
  | _ -> Buffer.add_string buf (Charamel_ansi.Style.to_sgr Charamel_ansi.Style.default));
  t.link_open <- None;
  t.emitted_style <- Some Charamel_ansi.Style.default

let write_run buf emitted_style link_open new_line lo hi =
  for index = lo to hi do
    if not new_line.(index).cont then begin
      let cell = new_line.(index) in
      if not (link_equal cell.link !link_open) then begin
        Buffer.add_string buf (Charamel_ansi.Link.osc8 cell.link);
        link_open := cell.link
      end;
      (match !emitted_style with
      | None -> Buffer.add_string buf (Charamel_ansi.Style.to_sgr cell.style)
      | Some style ->
          if not (Charamel_ansi.Style.equal style cell.style) then
            Buffer.add_string buf (Charamel_ansi.Style.transition ~from:style cell.style));
      emitted_style := Some cell.style;
      Buffer.add_string buf cell.text
    end
  done

let render_alt t buf (view : View.t) =
  if t.alt_needs_clear then begin
    reset_pen buf t;
    Buffer.add_string buf (Charamel_ansi.Seq.ed `All);
    Buffer.add_string buf (Charamel_ansi.Seq.cup ~row:1 ~col:1);
    t.alt_grid <- Array.init t.rows (fun _ -> Array.make t.cols blank);
    t.alt_needs_clear <- false;
    t.alt_pos <- Some (0, 0)
  end;
  let new_grid =
    pad_rows
      (layout ~width:t.cols ~max_rows:t.rows view.View.content)
      ~rows:t.rows ~width:t.cols
  in
  let emitted_style = ref t.emitted_style and link_open = ref t.link_open in
  let goto row col =
    if t.alt_pos <> Some (row, col) then begin
      Buffer.add_string buf (Charamel_ansi.Seq.cup ~row:(row + 1) ~col:(col + 1));
      t.alt_pos <- Some (row, col)
    end
  in
  if Array.length new_grid > 0 then begin
    for row = 0 to min (t.rows - 1) (Array.length new_grid - 1) do
      let old_line =
        if row < Array.length t.alt_grid then t.alt_grid.(row)
        else Array.make t.cols blank
      in
      match line_bounds ~force:false old_line new_grid.(row) with
      | None -> ()
      | Some (lo, hi, clear_tail) ->
          goto row lo;
          let erased_from = last_nonblank new_grid.(row) < lo in
          if erased_from then
            begin match !link_open with
            | Some _ ->
                Buffer.add_string buf (Charamel_ansi.Link.osc8 None);
                link_open := None
            | None -> ()
            end
          else write_run buf emitted_style link_open new_grid.(row) lo hi;
          if clear_tail then Buffer.add_string buf (Charamel_ansi.Seq.el `To_end);
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
      let col = snap_off_continuation new_grid row col in
      if t.alt_pos <> Some (row, col) then begin
        Buffer.add_string buf (Charamel_ansi.Seq.cup ~row:(row + 1) ~col:(col + 1));
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
    Buffer.add_string buf (Charamel_ansi.Seq.cuu (!row - to_row));
    row := to_row
  end;
  if to_col > !col then Buffer.add_string buf (Charamel_ansi.Seq.cuf (to_col - !col))
  else if to_col < !col then Buffer.add_string buf (Charamel_ansi.Seq.cub (!col - to_col))

let render_inline t buf (view : View.t) =
  let new_window = layout ~width:t.cols ~max_rows:t.rows view.View.content in
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
      else Array.make t.cols blank
    in
    let new_line = if row < h_new then new_window.(row) else Array.make t.cols blank in
    match line_bounds ~force:t.inline_poison old_line new_line with
    | None -> ()
    | Some (lo, hi, clear_tail) ->
        goto row lo;
        let erased_from = last_nonblank new_line < lo in
        if erased_from then
          begin match !link_open with
          | Some _ ->
              Buffer.add_string buf (Charamel_ansi.Link.osc8 None);
              link_open := None
          | None -> ()
          end
        else write_run buf emitted_style link_open new_line lo hi;
        if clear_tail then Buffer.add_string buf (Charamel_ansi.Seq.el `To_end);
        current_row := row;
        current_col := if erased_from then lo else hi + 1
  done;
  (match view.View.cursor with
  | Some cursor ->
      let row = if h_new <= 0 then 0 else max 0 (min cursor.Cursor.row (h_new - 1)) in
      let col = if t.cols <= 0 then 0 else max 0 (min cursor.Cursor.col (t.cols - 1)) in
      let col = snap_off_continuation new_window row col in
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
      Buffer.add_string buf Charamel_ansi.Seq.kitty_pop;
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
    Buffer.add_string buf (Charamel_ansi.Seq.kitty_push target);
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
      ((if visible then Charamel_ansi.Seq.decset else Charamel_ansi.Seq.decrst)
         Charamel_ansi.Seq.cursor_visible);
    t.cursor_visible <- visible
  end;
  (match cursor with
  | None -> ()
  | Some cursor ->
      let code = cursor_style_code cursor in
      if code <> t.cursor_shape_code then begin
        Buffer.add_string buf (if code = 1 then "\x1b[0 q" else Fmt.str "\x1b[%d q" code);
        t.cursor_shape_code <- code
      end);
  let target_color =
    match cursor with Some cursor -> applied_color cursor.Cursor.color | None -> None
  in
  if not (Option.equal Charamel_ansi.Color.equal target_color t.cursor_color) then begin
    (match target_color with
    | Some color -> (
        match hex_of_color color with
        | Some hex -> Buffer.add_string buf (Fmt.str "\x1b]12;%s\x1b\\" hex)
        | None -> ())
    | None -> Buffer.add_string buf "\x1b]112\x1b\\");
    t.cursor_color <- target_color
  end

let render t (view : View.t) =
  let buf = Buffer.create 256 in
  let entering_alt = view.View.alt_screen && not t.alt in
  let leaving_alt = (not view.View.alt_screen) && t.alt in
  let target_kflags = kitty_flags view.View.keyboard in
  if leaving_alt then begin
    if t.alt_pushed then begin
      Buffer.add_string buf Charamel_ansi.Seq.kitty_pop;
      t.alt_pushed <- false;
      t.alt_kflags <- 0
    end;
    Buffer.add_string buf (Charamel_ansi.Seq.decrst Charamel_ansi.Seq.alt_screen);
    t.alt <- false;
    apply_kitty buf t target_kflags;
    reset_pen buf t
  end
  else if entering_alt then begin
    Buffer.add_string buf (Charamel_ansi.Seq.decset Charamel_ansi.Seq.alt_screen);
    t.alt <- true;
    t.alt_grid <- Array.init t.rows (fun _ -> Array.make t.cols blank);
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
      ((if view.View.bracketed_paste then Charamel_ansi.Seq.decset
        else Charamel_ansi.Seq.decrst)
         Charamel_ansi.Seq.bracketed_paste);
    t.paste <- view.View.bracketed_paste
  end;
  if view.View.report_focus <> t.focus then begin
    Buffer.add_string buf
      ((if view.View.report_focus then Charamel_ansi.Seq.decset
        else Charamel_ansi.Seq.decrst)
         Charamel_ansi.Seq.focus);
    t.focus <- view.View.report_focus
  end;
  (match (view.View.title, t.title) with
  | Some title, previous when previous <> Some title ->
      Buffer.add_string buf (Charamel_ansi.Seq.title title);
      t.title <- Some title
  | None, Some _ ->
      Buffer.add_string buf (Charamel_ansi.Seq.title "");
      t.title <- None
  | _ -> ());
  let target_bg = applied_color view.View.background
  and target_fg = applied_color view.View.foreground in
  (match (target_bg, t.bg) with
  | Some color, previous
    when not (Option.equal Charamel_ansi.Color.equal (Some color) previous) ->
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
    when not (Option.equal Charamel_ansi.Color.equal (Some color) previous) ->
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
    Charamel_ansi.Seq.decset Charamel_ansi.Seq.sync_output
    ^ Buffer.contents buf
    ^ Charamel_ansi.Seq.decrst Charamel_ansi.Seq.sync_output

let move_to_anchor t anchor =
  let row = fst t.inline_pos in
  match anchor with
  | `Column_start ->
      t.inline_pos <- (row, 0);
      "\r"
  | `Fresh_line ->
      t.inline_pos <- (row + 1, 0);
      "\r\n"

let clear t =
  if t.alt || t.h_prev = 0 then ""
  else begin
    let buf = Buffer.create 64 in
    let current_row, current_col = t.inline_pos in
    move_inline buf ~from_row:current_row ~from_col:current_col ~to_row:0 ~to_col:0;
    for row = 0 to t.h_prev - 1 do
      Buffer.add_string buf (Charamel_ansi.Seq.el `All);
      if row < t.h_prev - 1 then Buffer.add_char buf '\n'
    done;
    if t.h_prev > 1 then Buffer.add_string buf (Charamel_ansi.Seq.cuu (t.h_prev - 1));
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
  Buffer.add_string buf (Charamel_ansi.Seq.decrst Charamel_ansi.Seq.sync_output);
  if t.alt then begin
    if t.alt_pushed then Buffer.add_string buf Charamel_ansi.Seq.kitty_pop;
    Buffer.add_string buf (Charamel_ansi.Seq.decrst Charamel_ansi.Seq.alt_screen)
  end;
  List.iter
    (fun mode -> Buffer.add_string buf (Charamel_ansi.Seq.decrst mode))
    (mouse_modes t.mouse);
  if t.paste then
    Buffer.add_string buf (Charamel_ansi.Seq.decrst Charamel_ansi.Seq.bracketed_paste);
  if t.focus then Buffer.add_string buf (Charamel_ansi.Seq.decrst Charamel_ansi.Seq.focus);
  if not t.cursor_visible then
    Buffer.add_string buf (Charamel_ansi.Seq.decset Charamel_ansi.Seq.cursor_visible);
  if t.cursor_shape_code <> 1 then Buffer.add_string buf "\x1b[0 q";
  (match t.title with
  | Some _ -> Buffer.add_string buf (Charamel_ansi.Seq.title "")
  | None -> ());
  (match t.bg with Some _ -> Buffer.add_string buf "\x1b]111\x07" | None -> ());
  (match t.fg with Some _ -> Buffer.add_string buf "\x1b]110\x07" | None -> ());
  (match t.cursor_color with
  | Some _ -> Buffer.add_string buf "\x1b]112\x1b\\"
  | None -> ());
  if t.progress <> View.Progress_none then
    Buffer.add_string buf (progress_osc View.Progress_none);
  (match t.link_open with
  | Some _ -> Buffer.add_string buf (Charamel_ansi.Link.osc8 None)
  | None -> ());
  Buffer.add_string buf (Charamel_ansi.Style.to_sgr Charamel_ansi.Style.default);
  if t.inline_pushed then Buffer.add_string buf Charamel_ansi.Seq.kitty_pop;
  reset t;
  Buffer.contents buf
