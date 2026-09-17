module Key = Charamel_tea.Key
module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color
module Text = Charamel_ansi.Text
module Width = Charamel_ansi.Width

let clamp n lo hi = max lo (min hi n)
let max_lines = 10_000

let is_control u =
  let n = Uchar.to_int u in
  (n >= 0 && n <= 0x1f && n <> 0x09 && n <> 0x0a && n <> 0x0d) || (n >= 0x7f && n <= 0x9f)

let sanitize s =
  let out = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then ()
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then loop (i + 1)
      else
        let u = Uchar.utf_decode_uchar d in
        let n = Uchar.utf_decode_length d in
        if Uchar.equal u (Uchar.of_char '\n') then Buffer.add_char out '\n'
        else if Uchar.equal u (Uchar.of_char '\r') then
          if i + n < String.length s && String.get s (i + n) = '\n' then ()
          else Buffer.add_char out '\n'
        else if Uchar.equal u (Uchar.of_char '\t') then Buffer.add_char out ' '
        else if not (is_control u) then Buffer.add_utf_8_uchar out u;
        loop (i + n)
  in
  loop 0;
  Buffer.contents out

let string_of_clusters xs = String.concat "" xs

let rec take n xs =
  if n <= 0 then [] else match xs with [] -> [] | x :: rest -> x :: take (n - 1) rest

let rec drop n xs =
  if n <= 0 then xs else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest

let sub xs start len = take len (drop start xs)
let cluster_width s = Width.grapheme_width s
let clusters_width xs = Stdlib.List.fold_left (fun n x -> n + cluster_width x) 0 xs
let style_inline s = Style.inline true s
let render_style style text = Style.render (style_inline style) text
let color n = match Color.indexed n with Some c -> c | None -> Color.Default
let light_dark ~is_dark light dark = Charamel_lipgloss.light_dark ~is_dark ~light ~dark

type style_state = {
  base : Style.t;
  text : Style.t;
  line_number : Style.t;
  cursor_line_number : Style.t;
  cursor_line : Style.t;
  end_of_buffer : Style.t;
  placeholder : Style.t;
  prompt : Style.t;
  selection : Style.t;
}

type cursor_style = Textinput.cursor_style
type styles = { focused : style_state; blurred : style_state; cursor : cursor_style }

let default_styles ~is_dark =
  let ld light dark = light_dark ~is_dark (color light) (color dark) in
  let focused =
    {
      base = Style.empty;
      text = Style.empty;
      line_number = Style.foreground (color 249) Style.empty;
      cursor_line_number = Style.foreground (ld 240 240) Style.empty;
      cursor_line = Style.background (ld 255 0) Style.empty;
      end_of_buffer = Style.foreground (ld 254 0) Style.empty;
      placeholder = Style.foreground (color 240) Style.empty;
      prompt = Style.foreground (color 7) Style.empty;
      selection = Style.background (ld 253 8) Style.empty;
    }
  in
  let blurred =
    {
      base = Style.empty;
      text = Style.foreground (ld 245 7) Style.empty;
      line_number = Style.foreground (ld 249 7) Style.empty;
      cursor_line_number = Style.foreground (ld 249 7) Style.empty;
      cursor_line = Style.foreground (ld 245 7) Style.empty;
      end_of_buffer = Style.foreground (ld 254 0) Style.empty;
      placeholder = Style.foreground (color 240) Style.empty;
      prompt = Style.foreground (color 7) Style.empty;
      selection = Style.background (ld 253 8) Style.empty;
    }
  in
  {
    focused;
    blurred;
    cursor =
      {
        color = color 7;
        shape = Charamel_tea.Cursor.Block;
        blink = true;
        blink_speed = None;
      };
  }

type keymap = {
  character_backward : Key_binding.t;
  character_forward : Key_binding.t;
  delete_after_cursor : Key_binding.t;
  delete_before_cursor : Key_binding.t;
  delete_character_backward : Key_binding.t;
  delete_character_forward : Key_binding.t;
  delete_word_backward : Key_binding.t;
  delete_word_forward : Key_binding.t;
  insert_newline : Key_binding.t;
  line_end : Key_binding.t;
  line_next : Key_binding.t;
  line_previous : Key_binding.t;
  line_start : Key_binding.t;
  page_up : Key_binding.t;
  page_down : Key_binding.t;
  paste : Key_binding.t;
  word_backward : Key_binding.t;
  word_forward : Key_binding.t;
  input_begin : Key_binding.t;
  input_end : Key_binding.t;
  uppercase_word_forward : Key_binding.t;
  lowercase_word_forward : Key_binding.t;
  capitalize_word_forward : Key_binding.t;
  transpose_character_backward : Key_binding.t;
  select_character_forward : Key_binding.t;
  select_character_backward : Key_binding.t;
  select_word_forward : Key_binding.t;
  select_word_backward : Key_binding.t;
  select_line_up : Key_binding.t;
  select_line_down : Key_binding.t;
  select_all : Key_binding.t;
  copy_selection : Key_binding.t;
}

let default_keymap =
  {
    character_backward = Key_binding.v [ "left"; "ctrl+b" ];
    character_forward = Key_binding.v [ "right"; "ctrl+f" ];
    delete_after_cursor = Key_binding.v [ "ctrl+k" ];
    delete_before_cursor = Key_binding.v [ "ctrl+u" ];
    delete_character_backward = Key_binding.v [ "backspace"; "ctrl+h" ];
    delete_character_forward = Key_binding.v [ "delete"; "ctrl+d" ];
    delete_word_backward = Key_binding.v [ "alt+backspace"; "ctrl+w"; "ctrl+backspace" ];
    delete_word_forward = Key_binding.v [ "alt+delete"; "alt+d"; "ctrl+delete" ];
    insert_newline = Key_binding.v [ "enter"; "ctrl+m" ];
    line_end = Key_binding.v [ "end"; "ctrl+e" ];
    line_next = Key_binding.v [ "down"; "ctrl+n" ];
    line_previous = Key_binding.v [ "up"; "ctrl+p" ];
    line_start = Key_binding.v [ "home"; "ctrl+a" ];
    page_up = Key_binding.v [ "pgup" ];
    page_down = Key_binding.v [ "pgdown" ];
    paste = Key_binding.v [ "ctrl+v" ];
    word_backward = Key_binding.v [ "alt+left"; "ctrl+left"; "alt+b" ];
    word_forward = Key_binding.v [ "alt+right"; "ctrl+right"; "alt+f" ];
    input_begin = Key_binding.v [ "alt+<"; "ctrl+home" ];
    input_end = Key_binding.v [ "alt+>"; "ctrl+end" ];
    uppercase_word_forward = Key_binding.v [ "alt+u" ];
    lowercase_word_forward = Key_binding.v [ "alt+l" ];
    capitalize_word_forward = Key_binding.v [ "alt+c" ];
    transpose_character_backward = Key_binding.v [ "ctrl+t" ];
    select_character_forward = Key_binding.v [ "shift+right" ];
    select_character_backward = Key_binding.v [ "shift+left" ];
    select_word_forward =
      Key_binding.v [ "ctrl+shift+right"; "alt+shift+right"; "alt+shift+f" ];
    select_word_backward =
      Key_binding.v [ "ctrl+shift+left"; "alt+shift+left"; "alt+shift+b" ];
    select_line_up = Key_binding.v [ "shift+up" ];
    select_line_down = Key_binding.v [ "shift+down" ];
    select_all = Key_binding.v [ "ctrl+g" ];
    copy_selection = Key_binding.v [ "ctrl+shift+c" ];
  }

type key_action =
  | Character_backward
  | Character_forward
  | Delete_after_cursor
  | Delete_before_cursor
  | Delete_character_backward
  | Delete_character_forward
  | Delete_word_backward
  | Delete_word_forward
  | Insert_newline
  | Line_end
  | Line_next
  | Line_previous
  | Line_start
  | Page_up
  | Page_down
  | Word_backward
  | Word_forward
  | Input_begin
  | Input_end
  | Uppercase_word_forward
  | Lowercase_word_forward
  | Capitalize_word_forward
  | Transpose_character_backward
  | Select_character_forward
  | Select_character_backward
  | Select_word_forward
  | Select_word_backward
  | Select_line_up
  | Select_line_down
  | Select_all

type msg =
  | Key_action of key_action
  | Insert of string
  | Copy_request of string
  | Paste_request
  | Cursor of Cursor.msg

type position = { row : int; col : int }
type prompt_info = { line_number : int; focused : bool }

type line_info = {
  width : int;
  char_width : int;
  height : int;
  start_column : int;
  column_offset : int;
  row_offset : int;
  char_offset : int;
}

type atom = Cluster of string | Newline

type visual_line = {
  logical : int;
  start : int;
  actual : string list;
  rendered : string list;
}

type t = {
  prompt : string;
  prompt_func : (int * (prompt_info -> string)) option;
  placeholder : string;
  show_line_numbers : bool;
  end_of_buffer_character : string;
  char_limit : int;
  max_height : int;
  max_width : int;
  width : int;
  height : int;
  dynamic_height : bool;
  min_height : int;
  max_content_height : int;
  keymap : keymap;
  styles : styles;
  virtual_cursor : bool;
  focused : bool;
  lines : string list list;
  row : int;
  col : int;
  sticky_column : int option;
  viewport : Viewport.t;
  selection_anchor : position option;
  selection_head : position option;
  selecting : bool;
  cursor : Cursor.t;
}

let active_style m = if m.focused then m.styles.focused else m.styles.blurred
let line_count m = max 1 (Stdlib.List.length m.lines)

let current_line m =
  match Stdlib.List.nth_opt m.lines m.row with Some x -> x | None -> []

let current_line_len m = Stdlib.List.length (current_line m)

let prompt_width m =
  match m.prompt_func with Some (w, _) -> w | None -> Text.width m.prompt

let digits n =
  let n = max 1 n in
  String.length (string_of_int n)

let line_number_width m = if m.show_line_numbers then digits m.max_height + 2 else 0
let gutter_width m = prompt_width m + line_number_width m

let content_width m =
  let available = if m.width > 0 then m.width - gutter_width m else m.max_width in
  max 1 (min m.max_width available)

let inherited ~parent child = style_inline (Style.inherit_ ~parent child)
let state_base st = inherited ~parent:st.base st.text
let state_cursor_line st = inherited ~parent:st.base st.cursor_line
let state_line_number (st : style_state) = inherited ~parent:st.base st.line_number
let state_cursor_line_number st = inherited ~parent:st.base st.cursor_line_number
let state_placeholder (st : style_state) = inherited ~parent:st.base st.placeholder
let state_prompt (st : style_state) = inherited ~parent:st.base st.prompt
let state_eob st = inherited ~parent:st.base st.end_of_buffer
let state_selection st = inherited ~parent:(state_base st) st.selection

let cursor_mode (styles : cursor_style) virtual_cursor =
  if not virtual_cursor then Cursor.Hide
  else if styles.blink then Cursor.Blink
  else Cursor.Static

let sync_cursor m =
  let cs = m.styles.cursor in
  let mode = cursor_mode cs m.virtual_cursor in
  let st = active_style m in
  let c = Cursor.set_style (Style.foreground cs.color Style.empty) m.cursor in
  let c = Cursor.set_text_style (state_cursor_line st) c in
  let c = match cs.blink_speed with Some s -> Cursor.set_blink_speed s c | None -> c in
  let c = if Cursor.mode c = mode then c else Cursor.set_mode mode c in
  let c =
    if Cursor.focused c = m.focused then c
    else if m.focused then Cursor.focus c
    else Cursor.blur c
  in
  { m with cursor = c }

let line_split s =
  let lines = String.split_on_char '\n' s |> Stdlib.List.map Width.graphemes in
  match lines with [] -> [ [] ] | lines -> lines

let atoms_of_lines lines =
  let rec add_line acc = function
    | [] -> acc
    | [ line ] -> Stdlib.List.rev_append (Stdlib.List.map (fun x -> Cluster x) line) acc
    | line :: rest ->
        let acc =
          Stdlib.List.rev_append (Stdlib.List.map (fun x -> Cluster x) line) acc
        in
        add_line (Newline :: acc) rest
  in
  Stdlib.List.rev (add_line [] lines)

let lines_of_atoms atoms =
  let rec loop current acc = function
    | [] -> Stdlib.List.rev (Stdlib.List.rev current :: acc)
    | Cluster x :: rest -> loop (x :: current) acc rest
    | Newline :: rest -> loop [] (Stdlib.List.rev current :: acc) rest
  in
  match loop [] [] atoms with [] -> [ [] ] | rows -> rows

let atom_unit = function Cluster x -> Width.grapheme_width x | Newline -> 1
let atoms_units atoms = Stdlib.List.fold_left (fun n atom -> n + atom_unit atom) 0 atoms

let rec take_atoms_units budget used acc = function
  | [] -> Stdlib.List.rev acc
  | atom :: rest ->
      let amount = atom_unit atom in
      if amount <= 0 || used + amount > budget then Stdlib.List.rev acc
      else take_atoms_units budget (used + amount) (atom :: acc) rest

let position_index lines (p : position) =
  let rec loop row acc = function
    | [] -> acc
    | line :: rest ->
        if row = p.row then acc + min p.col (Stdlib.List.length line)
        else loop (row + 1) (acc + Stdlib.List.length line + 1) rest
  in
  loop 0 0 lines

let index_position lines index =
  let rec loop row remaining = function
    | [] -> { row = max 0 (row - 1); col = 0 }
    | line :: rest ->
        let len = Stdlib.List.length line in
        if remaining <= len then { row; col = max 0 remaining }
        else loop (row + 1) (remaining - len - 1) rest
  in
  loop 0 (max 0 index) lines

let normalize_position m (p : position) =
  let row = clamp p.row 0 (line_count m - 1) in
  let line = match Stdlib.List.nth_opt m.lines row with Some x -> x | None -> [] in
  { row; col = clamp p.col 0 (Stdlib.List.length line) }

let pos_compare (a : position) (b : position) =
  if a.row <> b.row then compare a.row b.row else compare a.col b.col

let normalized_selection m =
  match (m.selection_anchor, m.selection_head) with
  | Some a, Some b when a <> b ->
      let a = normalize_position m a and b = normalize_position m b in
      if pos_compare a b <= 0 then Some (a, b) else Some (b, a)
  | _ -> None

let selection m = normalized_selection m
let has_selection m = match normalized_selection m with Some _ -> true | None -> false

let selected_text m =
  match normalized_selection m with
  | None -> ""
  | Some (start, stop) ->
      if start.row = stop.row then
        string_of_clusters
          (sub (Stdlib.List.nth m.lines start.row) start.col (stop.col - start.col))
      else
        let first =
          string_of_clusters (drop start.col (Stdlib.List.nth m.lines start.row))
        in
        let middle =
          Stdlib.List.init
            (stop.row - start.row - 1)
            (fun i -> string_of_clusters (Stdlib.List.nth m.lines (start.row + i + 1)))
        in
        let last =
          string_of_clusters (take stop.col (Stdlib.List.nth m.lines stop.row))
        in
        String.concat "\n" ((first :: middle) @ [ last ])

let clear_selection m =
  { m with selection_anchor = None; selection_head = None; selecting = false }

let select_from anchor head m =
  let anchor = normalize_position m anchor and head = normalize_position m head in
  {
    m with
    selection_anchor = Some anchor;
    selection_head = Some head;
    selecting = true;
    row = head.row;
    col = head.col;
  }

let selected_indices_for_line m row start len =
  match normalized_selection m with
  | None -> None
  | Some (a, b) when row < a.row || row > b.row -> None
  | Some (a, b) ->
      let lo = if row = a.row then a.col else 0 in
      let hi =
        if row = b.row then b.col else Stdlib.List.length (Stdlib.List.nth m.lines row)
      in
      let lo = max lo start and hi = min hi (start + len) in
      if lo >= hi then None else Some (lo - start, hi - start)

let sanitized_atoms s =
  let s = sanitize s in
  let lines = line_split s in
  atoms_of_lines lines

let whitespace_cluster s =
  if s = "" then false
  else
    let d = String.get_utf_8_uchar s 0 in
    Uchar.utf_decode_is_valid d && Uucp.White.is_white_space (Uchar.utf_decode_uchar d)

let rec tokenize_words start acc = function
  | [] -> Stdlib.List.rev acc
  | xs ->
      let is_space = whitespace_cluster (Stdlib.List.hd xs) in
      let rec take_same n = function
        | y :: ys when whitespace_cluster y = is_space -> take_same (y :: n) ys
        | rest -> (Stdlib.List.rev n, rest)
      in
      let token, rest = take_same [] xs in
      tokenize_words
        (start + Stdlib.List.length token)
        ((start, token, is_space) :: acc)
        rest

let wrap_line clusters width =
  if clusters = [] then [ { logical = 0; start = 0; actual = []; rendered = [] } ]
  else if width <= 0 then
    [ { logical = 0; start = 0; actual = clusters; rendered = clusters } ]
  else
    let tokens = tokenize_words 0 [] clusters in
    let rows = ref [] in
    let current = ref [] in
    let current_start = ref 0 in
    let current_width = ref 0 in
    let flush () =
      if !current <> [] then (
        rows :=
          { logical = 0; start = !current_start; actual = !current; rendered = !current }
          :: !rows;
        current := [];
        current_width := 0)
    in
    let rec consume pending = function
      | [] ->
          if pending <> [] then
            if !current_width + clusters_width pending <= width then
              current := !current @ pending
            else (
              flush ();
              current := pending;
              current_start := 0;
              current_width := clusters_width pending);
          flush ()
      | (_, token, true) :: rest -> consume (pending @ token) rest
      | (start, token, false) :: rest ->
          let token_width = clusters_width token in
          if !current = [] then (
            current := pending @ token;
            current_start := start - Stdlib.List.length pending;
            current_width := clusters_width !current)
          else if !current_width + clusters_width pending + token_width <= width then (
            current := !current @ pending @ token;
            current_width := !current_width + clusters_width pending + token_width)
          else (
            flush ();
            current := pending @ token;
            current_start := start - Stdlib.List.length pending;
            current_width := clusters_width !current);
          consume [] rest
    in
    consume [] tokens;
    let rows = Stdlib.List.rev !rows in
    let rec split_row row start xs =
      match xs with
      | [] -> []
      | _ ->
          let rec fit used acc = function
            | [] -> (Stdlib.List.rev acc, [])
            | x :: rest ->
                let w = cluster_width x in
                if acc <> [] && used + w > width then (Stdlib.List.rev acc, x :: rest)
                else if acc = [] && w > width then ([ x ], rest)
                else fit (used + w) (x :: acc) rest
          in
          let chunk, rest = fit 0 [] xs in
          let current = { row with start; actual = chunk; rendered = chunk } in
          current :: split_row row (start + Stdlib.List.length chunk) rest
    in
    let hardened =
      Stdlib.List.concat_map
        (fun row ->
          if clusters_width row.actual <= width then [ row ]
          else split_row row row.start row.actual)
        rows
    in
    match hardened with
    | [] -> [ { logical = 0; start = 0; actual = clusters; rendered = clusters } ]
    | rows -> rows

let visual_lines m =
  let width = content_width m in
  let rec each logical acc = function
    | [] -> Stdlib.List.rev acc
    | line :: rest ->
        let wrapped = wrap_line line width in
        let wrapped = Stdlib.List.map (fun row -> { row with logical }) wrapped in
        each (logical + 1) (Stdlib.List.rev_append wrapped acc) rest
  in
  each 0 [] m.lines

let total_visual_lines m = Stdlib.List.length (visual_lines m)

let visual_location m =
  let rows = visual_lines m in
  let rec loop index = function
    | [] -> (0, 0, 0, 0)
    | v :: rest
      when v.logical = m.row && m.col >= v.start
           && (v.actual = [] || m.col < v.start + Stdlib.List.length v.actual || rest = [])
      ->
        let before = take (max 0 (m.col - v.start)) v.actual in
        (index, clusters_width before, v.start, Stdlib.List.length v.actual)
    | _ :: rest -> loop (index + 1) rest
  in
  loop 0 rows

let visual_at rows index = Stdlib.List.nth_opt rows index

let line_info m =
  let rows = visual_lines m in
  let visual_row, char_offset, start, _ = visual_location m in
  let current_rows = Stdlib.List.filter (fun x -> x.logical = m.row) rows in
  let first_row =
    let rec find index = function
      | [] -> visual_row
      | row :: rest -> if row.logical = m.row then index else find (index + 1) rest
    in
    find 0 rows
  in
  let height = max 1 (Stdlib.List.length current_rows) in
  let width =
    match Stdlib.List.nth_opt rows visual_row with
    | Some row -> clusters_width row.actual
    | None -> 0
  in
  {
    width;
    char_width = width;
    height;
    start_column = start;
    column_offset = max 0 (m.col - start);
    row_offset = max 0 (visual_row - first_row);
    char_offset;
  }

let set_position ?(clear = false) p m =
  let p = normalize_position m p in
  let m = { m with row = p.row; col = p.col; sticky_column = None } in
  if clear then clear_selection m else m

let rebuild_viewport m =
  let rows = visual_lines m in
  let old_y = Viewport.y_offset m.viewport in
  let vp = Viewport.set_width (content_width m) m.viewport in
  let vp = Viewport.set_height m.height vp in
  let vp =
    Viewport.set_content_lines
      (Stdlib.List.map (fun x -> string_of_clusters x.rendered) rows)
      vp
  in
  let vp = Viewport.set_y_offset old_y vp in
  let visual_row, col_offset, _, _ = visual_location m in
  let vp =
    Viewport.ensure_visible ~line:visual_row ~colstart:col_offset ~colend:(col_offset + 1)
      vp
  in
  { m with viewport = vp }

let recalc_height m =
  if not m.dynamic_height then m
  else
    let wanted = clamp (total_visual_lines m) m.min_height m.max_height in
    { m with height = wanted }

let rebuild m = m |> recalc_height |> sync_cursor |> rebuild_viewport

let v ?(prompt = "┃ ") ?(placeholder = "") ?(show_line_numbers = true)
    ?(end_of_buffer_character = " ") ?(char_limit = 0) ?(max_height = 99)
    ?(max_width = 500) ?(width = 40) ?(height = 6) ?(dynamic_height = false)
    ?(min_height = 1) ?(max_content_height = 0) ?(keymap = default_keymap)
    ?(is_dark = true) ?styles ?(virtual_cursor = true) ?(value = "") () =
  let styles = match styles with Some s -> s | None -> default_styles ~is_dark in
  let max_height = max 1 (min max_lines max_height) in
  let min_height = clamp min_height 1 max_height in
  let height = clamp height min_height max_height in
  let lines = line_split (sanitize value) in
  let lines = take max_lines lines in
  let base =
    {
      prompt;
      prompt_func = None;
      placeholder;
      show_line_numbers;
      end_of_buffer_character;
      char_limit = max 0 char_limit;
      max_height;
      max_width = max 1 max_width;
      width = max 0 width;
      height;
      dynamic_height;
      min_height;
      max_content_height = max 0 max_content_height;
      keymap;
      styles;
      virtual_cursor;
      focused = false;
      lines = [ [] ];
      row = 0;
      col = 0;
      sticky_column = None;
      viewport = Viewport.v ~width:0 ~height ();
      selection_anchor = None;
      selection_head = None;
      selecting = false;
      cursor = Cursor.v ();
    }
  in
  let atoms = atoms_of_lines lines in
  let atoms =
    if base.char_limit > 0 then take_atoms_units base.char_limit 0 [] atoms else atoms
  in
  let m = { base with lines = lines_of_atoms atoms } in
  let m = { m with row = 0; col = Stdlib.List.length (Stdlib.List.hd m.lines) } in
  rebuild m

let value m = String.concat "\n" (Stdlib.List.map string_of_clusters m.lines)

let length m =
  let line_width =
    Stdlib.List.fold_left (fun n line -> n + clusters_width line) 0 m.lines
  in
  line_width + max 0 (line_count m - 1)

let line m = m.row
let column m = m.col
let width m = m.width
let height m = m.height
let focused m = m.focused
let keymap m = m.keymap
let styles m = m.styles

let set_value s m =
  let lines = line_split (sanitize s) |> take max_lines in
  let atoms = atoms_of_lines lines in
  let atoms =
    if m.char_limit > 0 then take_atoms_units m.char_limit 0 [] atoms else atoms
  in
  let lines = lines_of_atoms atoms in
  let candidate =
    {
      m with
      lines;
      row = 0;
      col = 0;
      selection_anchor = None;
      selection_head = None;
      selecting = false;
    }
  in
  let candidate =
    {
      candidate with
      row = line_count candidate - 1;
      col = Stdlib.List.length (current_line candidate);
    }
  in
  if m.max_content_height > 0 && total_visual_lines candidate > m.max_content_height then
    m
  else rebuild candidate

let replace_range start stop inserted m =
  let atoms = atoms_of_lines m.lines in
  let i = position_index m.lines start and j = position_index m.lines stop in
  let before = take i atoms and after = drop j atoms in
  let available =
    if m.char_limit > 0 then max 0 (m.char_limit - atoms_units before - atoms_units after)
    else atoms_units inserted
  in
  let inserted = take_atoms_units available 0 [] inserted in
  let candidate_atoms = before @ inserted @ after in
  let candidate_lines = lines_of_atoms candidate_atoms in
  if Stdlib.List.length candidate_lines > max_lines then m
  else if m.max_content_height > 0 then
    let candidate = { m with lines = candidate_lines } in
    if total_visual_lines candidate > m.max_content_height then m else candidate
  else { m with lines = candidate_lines }

let insert_string s m =
  let start, stop =
    match normalized_selection m with
    | Some (a, b) -> (a, b)
    | None -> ({ row = m.row; col = m.col }, { row = m.row; col = m.col })
  in
  let inserted = sanitized_atoms s in
  let source = atoms_of_lines m.lines in
  let i = position_index m.lines start and j = position_index m.lines stop in
  let before = take i source and after = drop j source in
  let available =
    if m.char_limit > 0 then max 0 (m.char_limit - atoms_units before - atoms_units after)
    else atoms_units inserted
  in
  let accepted = take_atoms_units available 0 [] inserted in
  let m' = replace_range start stop accepted m in
  if m'.lines = m.lines then m
  else
    let idx = position_index m'.lines start + Stdlib.List.length accepted in
    let p = index_position m'.lines idx in
    rebuild
      {
        m' with
        row = p.row;
        col = p.col;
        selection_anchor = None;
        selection_head = None;
        selecting = false;
        sticky_column = None;
      }

let paste s m = insert_string s m

let delete_selection m =
  match normalized_selection m with
  | None -> m
  | Some (start, stop) ->
      let atoms = atoms_of_lines m.lines in
      let i = position_index m.lines start and j = position_index m.lines stop in
      let lines = lines_of_atoms (take i atoms @ drop j atoms) in
      rebuild
        {
          m with
          lines;
          row = start.row;
          col = start.col;
          selection_anchor = None;
          selection_head = None;
          selecting = false;
        }

let reset m =
  rebuild
    {
      m with
      lines = [ [] ];
      row = 0;
      col = 0;
      selection_anchor = None;
      selection_head = None;
      selecting = false;
      sticky_column = None;
    }

let character_forward m =
  if m.col < current_line_len m then set_position { row = m.row; col = m.col + 1 } m
  else if m.row + 1 < line_count m then set_position { row = m.row + 1; col = 0 } m
  else m

let character_backward m =
  if m.col > 0 then set_position { row = m.row; col = m.col - 1 } m
  else if m.row > 0 then
    set_position
      { row = m.row - 1; col = Stdlib.List.length (Stdlib.List.nth m.lines (m.row - 1)) }
      m
  else m

let word_backward m =
  if m.col = 0 then character_backward m
  else
    let line = current_line m in
    let i = ref m.col in
    while !i > 0 && whitespace_cluster (Stdlib.List.nth line (!i - 1)) do
      decr i
    done;
    while !i > 0 && not (whitespace_cluster (Stdlib.List.nth line (!i - 1))) do
      decr i
    done;
    set_position { row = m.row; col = !i } m

let word_forward m =
  let line = current_line m in
  let i = ref m.col in
  while !i < Stdlib.List.length line && whitespace_cluster (Stdlib.List.nth line !i) do
    incr i
  done;
  while
    !i < Stdlib.List.length line && not (whitespace_cluster (Stdlib.List.nth line !i))
  do
    incr i
  done;
  if !i = Stdlib.List.length line && m.row + 1 < line_count m then
    set_position { row = m.row + 1; col = 0 } m
  else set_position { row = m.row; col = !i } m

let delete_character_backward m =
  if has_selection m then delete_selection m
  else if m.col > 0 then
    delete_selection
      (select_from { row = m.row; col = m.col - 1 } { row = m.row; col = m.col } m)
  else if m.row > 0 then
    delete_selection
      (select_from
         {
           row = m.row - 1;
           col = Stdlib.List.length (Stdlib.List.nth m.lines (m.row - 1));
         }
         { row = m.row; col = 0 } m)
  else m

let delete_character_forward m =
  if has_selection m then delete_selection m
  else if m.col < current_line_len m then
    delete_selection
      (select_from { row = m.row; col = m.col } { row = m.row; col = m.col + 1 } m)
  else if m.row + 1 < line_count m then
    delete_selection
      (select_from { row = m.row; col = m.col } { row = m.row + 1; col = 0 } m)
  else m

let delete_word_backward m =
  if has_selection m then delete_selection m
  else
    let target = word_backward m in
    if target.row = m.row && target.col = m.col then m
    else
      delete_selection
        (select_from
           { row = target.row; col = target.col }
           { row = m.row; col = m.col } m)

let delete_word_forward m =
  if has_selection m then delete_selection m
  else
    let target = word_forward m in
    if target.row = m.row && target.col = m.col then m
    else
      delete_selection
        (select_from { row = m.row; col = m.col }
           { row = target.row; col = target.col }
           m)

let line_start m = set_position ~clear:true { row = m.row; col = 0 } m
let line_end m = set_position ~clear:true { row = m.row; col = current_line_len m } m
let move_to_begin m = set_position ~clear:true { row = 0; col = 0 } m

let move_to_end m =
  set_position ~clear:true
    {
      row = line_count m - 1;
      col = Stdlib.List.length (Stdlib.List.nth m.lines (line_count m - 1));
    }
    m

let visual_col m =
  let _, offset, _, _ = visual_location m in
  offset

let position_for_visual m target desired =
  let rows = visual_lines m in
  match visual_at rows target with
  | None ->
      if target <= 0 then { row = 0; col = 0 }
      else
        {
          row = line_count m - 1;
          col = Stdlib.List.length (Stdlib.List.nth m.lines (line_count m - 1));
        }
  | Some v ->
      let rec find col used = function
        | [] -> col
        | x :: xs ->
            if used + cluster_width x > desired then col
            else find (col + 1) (used + cluster_width x) xs
      in
      { row = v.logical; col = v.start + find 0 0 v.actual }

let move_visual delta m =
  let current, _, _, _ = visual_location m in
  let target = clamp (current + delta) 0 (max 0 (total_visual_lines m - 1)) in
  let desired = match m.sticky_column with Some x -> x | None -> visual_col m in
  let p = position_for_visual m target desired in
  set_position
    { p with col = min p.col (Stdlib.List.length (Stdlib.List.nth m.lines p.row)) }
    m
  |> fun m -> { m with sticky_column = Some desired }

let cursor_down m = rebuild (move_visual 1 m)
let cursor_up m = rebuild (move_visual (-1) m)
let set_cursor_column col m = rebuild (set_position ~clear:true { row = m.row; col } m)
let cursor_start m = rebuild (set_cursor_column 0 m)
let cursor_end m = rebuild (set_cursor_column (current_line_len m) m)
let page_up m = rebuild (move_visual (-max 1 m.height) m)
let page_down m = rebuild (move_visual (max 1 m.height) m)

let lower_string s =
  let out = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then ()
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then loop (i + 1)
      else
        let u = Uchar.utf_decode_uchar d in
        let n = Uchar.utf_decode_length d in
        (match Uucp.Case.Map.to_lower u with
        | `Self -> Buffer.add_utf_8_uchar out u
        | `Uchars xs -> Stdlib.List.iter (Buffer.add_utf_8_uchar out) xs);
        loop (i + n)
  in
  loop 0;
  Buffer.contents out

let upper_string s =
  let out = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then ()
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then loop (i + 1)
      else
        let u = Uchar.utf_decode_uchar d in
        let n = Uchar.utf_decode_length d in
        (match Uucp.Case.Map.to_upper u with
        | `Self -> Buffer.add_utf_8_uchar out u
        | `Uchars xs -> Stdlib.List.iter (Buffer.add_utf_8_uchar out) xs);
        loop (i + n)
  in
  loop 0;
  Buffer.contents out

let word_bounds m =
  let line = current_line m in
  let i = ref m.col in
  while !i < Stdlib.List.length line && whitespace_cluster (Stdlib.List.nth line !i) do
    incr i
  done;
  let start = !i in
  while
    !i < Stdlib.List.length line && not (whitespace_cluster (Stdlib.List.nth line !i))
  do
    incr i
  done;
  (start, !i)

let word m =
  let line = current_line m in
  if m.col >= Stdlib.List.length line || whitespace_cluster (Stdlib.List.nth line m.col)
  then ""
  else
    let start = ref m.col in
    let stop = ref m.col in
    while !start > 0 && not (whitespace_cluster (Stdlib.List.nth line (!start - 1))) do
      decr start
    done;
    while
      !stop < Stdlib.List.length line
      && not (whitespace_cluster (Stdlib.List.nth line !stop))
    do
      incr stop
    done;
    string_of_clusters (sub line !start (!stop - !start))

let transform_word f m =
  let start, stop = word_bounds m in
  if start = stop then m
  else
    let line = current_line m in
    let word = string_of_clusters (sub line start (stop - start)) in
    let replacement = Width.graphemes (f word) in
    let lines =
      Stdlib.List.mapi
        (fun i xs ->
          if i = m.row then sub xs 0 start @ replacement @ drop stop xs else xs)
        m.lines
    in
    rebuild { m with lines; col = start + Stdlib.List.length replacement }

let transpose m =
  if m.col = 0 then m
  else
    let line = current_line m in
    let i, j =
      if m.col = Stdlib.List.length line then (m.col - 2, m.col - 1)
      else (m.col - 1, m.col)
    in
    if i < 0 || j >= Stdlib.List.length line then m
    else
      let swapped =
        Stdlib.List.mapi
          (fun n x ->
            if n = i then Stdlib.List.nth line j
            else if n = j then Stdlib.List.nth line i
            else x)
          line
      in
      rebuild
        {
          m with
          lines = Stdlib.List.mapi (fun n x -> if n = m.row then swapped else x) m.lines;
          col = min (Stdlib.List.length line) (j + 1);
        }

let insert_newline m = insert_string "\n" m
let action_move f m = if has_selection m then f (clear_selection m) else f m

let apply_action action m =
  match action with
  | Character_backward -> action_move character_backward m
  | Character_forward -> action_move character_forward m
  | Delete_after_cursor ->
      if has_selection m then delete_selection m
      else
        delete_selection
          (select_from { row = m.row; col = m.col }
             { row = m.row; col = current_line_len m }
             m)
  | Delete_before_cursor ->
      if has_selection m then delete_selection m
      else
        delete_selection
          (select_from { row = m.row; col = 0 } { row = m.row; col = m.col } m)
  | Delete_character_backward -> delete_character_backward m
  | Delete_character_forward -> delete_character_forward m
  | Delete_word_backward -> delete_word_backward m
  | Delete_word_forward -> delete_word_forward m
  | Insert_newline -> insert_newline m
  | Line_end -> action_move line_end m
  | Line_next -> action_move cursor_down m
  | Line_previous -> action_move cursor_up m
  | Line_start -> action_move line_start m
  | Page_up -> action_move page_up m
  | Page_down -> action_move page_down m
  | Word_backward -> action_move word_backward m
  | Word_forward -> action_move word_forward m
  | Input_begin -> action_move move_to_begin m
  | Input_end -> action_move move_to_end m
  | Uppercase_word_forward -> transform_word upper_string m
  | Lowercase_word_forward -> transform_word lower_string m
  | Capitalize_word_forward ->
      transform_word
        (fun word ->
          match Width.graphemes word with
          | [] -> ""
          | first :: rest -> upper_string first ^ lower_string (String.concat "" rest))
        m
  | Transpose_character_backward -> transpose m
  | Select_character_forward ->
      let anchor =
        match m.selection_anchor with Some x -> x | None -> { row = m.row; col = m.col }
      in
      let m = character_forward m in
      select_from anchor { row = m.row; col = m.col } m
  | Select_character_backward ->
      let anchor =
        match m.selection_anchor with Some x -> x | None -> { row = m.row; col = m.col }
      in
      let m = character_backward m in
      select_from anchor { row = m.row; col = m.col } m
  | Select_word_forward ->
      let anchor =
        match m.selection_anchor with Some x -> x | None -> { row = m.row; col = m.col }
      in
      let m = word_forward m in
      select_from anchor { row = m.row; col = m.col } m
  | Select_word_backward ->
      let anchor =
        match m.selection_anchor with Some x -> x | None -> { row = m.row; col = m.col }
      in
      let m = word_backward m in
      select_from anchor { row = m.row; col = m.col } m
  | Select_line_up ->
      let anchor =
        match m.selection_anchor with Some x -> x | None -> { row = m.row; col = m.col }
      in
      let m = cursor_up m in
      select_from anchor { row = m.row; col = m.col } m
  | Select_line_down ->
      let anchor =
        match m.selection_anchor with Some x -> x | None -> { row = m.row; col = m.col }
      in
      let m = cursor_down m in
      select_from anchor { row = m.row; col = m.col } m
  | Select_all ->
      select_from { row = 0; col = 0 }
        {
          row = line_count m - 1;
          col = Stdlib.List.length (Stdlib.List.nth m.lines (line_count m - 1));
        }
        m

let update message m =
  if not m.focused then (m, Cmd.none)
  else
    let old = (m.row, m.col) in
    let m =
      match message with
      | Cursor msg -> { m with cursor = Cursor.update msg m.cursor }
      | Copy_request _ | Paste_request -> m
      | Insert s -> insert_string s m
      | Key_action action -> apply_action action m
    in
    let m =
      if old <> (m.row, m.col) then { m with cursor = Cursor.show m.cursor } else m
    in
    (rebuild m, Cmd.none)

let prompt_for m line_number =
  match m.prompt_func with
  | None -> m.prompt
  | Some (w, fn) ->
      let p = fn { line_number; focused = m.focused } in
      let gap = max 0 (w - Text.width p) in
      String.make gap ' ' ^ p

let number_view m number cursor_line =
  if not m.show_line_numbers then ""
  else
    let s = if number <= 0 then " " else string_of_int number in
    let text =
      " " ^ String.make (max 0 (digits m.max_height - String.length s)) ' ' ^ s ^ " "
    in
    let st = active_style m in
    let number_style =
      if cursor_line then state_cursor_line_number st else state_line_number st
    in
    render_style number_style text

let render_cursor m char text_style =
  Cursor.view (Cursor.set_text_style text_style (Cursor.set_char char m.cursor))

let render_content_line m visual_index v =
  let st = active_style m in
  let cursor_line = v.logical = m.row in
  let base_style = if cursor_line then state_cursor_line st else state_base st in
  let selection_style = state_selection st in
  let actual = v.actual in
  let pieces = Buffer.create 64 in
  let cursor_visual, _, _, _ = visual_location m in
  let rec add i =
    if i >= Stdlib.List.length actual then ()
    else
      let cluster = Stdlib.List.nth actual i in
      let selected =
        match selected_indices_for_line m v.logical (v.start + i) 1 with
        | Some (a, b) -> i >= a && i < b
        | None -> false
      in
      if
        m.virtual_cursor && m.focused && cursor_visual = visual_index
        && (not (has_selection m))
        && m.col = v.start + i
      then Buffer.add_string pieces (render_cursor m cluster base_style)
      else
        Buffer.add_string pieces
          (render_style (if selected then selection_style else base_style) cluster);
      add (i + 1)
  in
  add 0;
  if
    m.virtual_cursor && m.focused && cursor_visual = visual_index
    && (not (has_selection m))
    && m.col >= v.start + Stdlib.List.length actual
  then Buffer.add_string pieces (render_cursor m " " base_style);
  let raw_width = clusters_width actual in
  let content = Buffer.contents pieces in
  let content =
    content
    ^ render_style base_style (String.make (max 0 (content_width m - raw_width)) ' ')
  in
  let prompt = render_style (state_prompt st) (prompt_for m visual_index) in
  let number =
    if v.start = 0 then number_view m (v.logical + 1) cursor_line
    else number_view m 0 cursor_line
  in
  prompt ^ number ^ content

let placeholder_rows m =
  let st = active_style m in
  let clusters = Width.graphemes m.placeholder in
  let wrapped = wrap_line clusters (content_width m) in
  let rec render i acc =
    if i >= m.height then Stdlib.List.rev acc
    else if i < Stdlib.List.length wrapped then
      let v = Stdlib.List.nth wrapped i in
      let cursor_line = i = 0 in
      let base_style =
        if cursor_line then state_cursor_line st else state_placeholder st
      in
      let text = String.concat "" v.actual in
      let content =
        if m.virtual_cursor && m.focused && i = 0 then
          match v.actual with
          | first :: rest ->
              render_cursor m first (state_placeholder st)
              ^ render_style (state_placeholder st) (String.concat "" rest)
          | [] -> render_cursor m " " (state_placeholder st)
        else render_style base_style text
      in
      let content =
        content
        ^ render_style base_style
            (String.make (max 0 (content_width m - Text.width text)) ' ')
      in
      let prompt = render_style (state_prompt st) (prompt_for m i) in
      let number =
        if i = 0 then number_view m 1 true
        else if m.show_line_numbers then number_view m 0 true
        else ""
      in
      render (i + 1) ((prompt ^ number ^ content) :: acc)
    else
      let prompt = render_style (state_prompt st) (prompt_for m i) in
      let eob = render_style (state_eob st) m.end_of_buffer_character in
      render (i + 1)
        ((prompt ^ eob
         ^ String.make
             (max 0 (content_width m - Text.width m.end_of_buffer_character))
             ' ')
        :: acc)
  in
  render 0 []

let view m =
  let m = rebuild m in
  if m.lines = [ [] ] && m.placeholder <> "" then
    Style.render (active_style m).base (String.concat "\n" (placeholder_rows m))
  else
    let rows = visual_lines m in
    let start = Viewport.y_offset m.viewport in
    let visible =
      rows
      |> Stdlib.List.mapi (fun i row -> (i, row))
      |> Stdlib.List.filter (fun (i, _) -> i >= start && i < start + m.height)
    in
    let rendered =
      Stdlib.List.map (fun (i, row) -> render_content_line m i row) visible
    in
    let missing = max 0 (m.height - Stdlib.List.length rendered) in
    let st = active_style m in
    let eob =
      Stdlib.List.init missing (fun i ->
          let prompt =
            render_style (state_prompt st)
              (prompt_for m (start + Stdlib.List.length rendered + i))
          in
          let marker = render_style (state_eob st) m.end_of_buffer_character in
          prompt ^ marker
          ^ String.make
              (max 0 (content_width m - Text.width m.end_of_buffer_character))
              ' ')
    in
    Style.render st.base (String.concat "\n" (rendered @ eob))

let key (m : t) (key : Key.t) =
  if not m.focused then None
  else
    let hit b = Key_binding.matches key b in
    if hit m.keymap.character_backward then Some (Key_action Character_backward)
    else if hit m.keymap.character_forward then Some (Key_action Character_forward)
    else if hit m.keymap.delete_after_cursor then Some (Key_action Delete_after_cursor)
    else if hit m.keymap.delete_before_cursor then Some (Key_action Delete_before_cursor)
    else if hit m.keymap.delete_character_backward then
      Some (Key_action Delete_character_backward)
    else if hit m.keymap.delete_character_forward then
      Some (Key_action Delete_character_forward)
    else if hit m.keymap.delete_word_backward then Some (Key_action Delete_word_backward)
    else if hit m.keymap.delete_word_forward then Some (Key_action Delete_word_forward)
    else if hit m.keymap.insert_newline then Some (Key_action Insert_newline)
    else if hit m.keymap.line_end then Some (Key_action Line_end)
    else if hit m.keymap.line_next then Some (Key_action Line_next)
    else if hit m.keymap.line_previous then Some (Key_action Line_previous)
    else if hit m.keymap.line_start then Some (Key_action Line_start)
    else if hit m.keymap.page_up then Some (Key_action Page_up)
    else if hit m.keymap.page_down then Some (Key_action Page_down)
    else if hit m.keymap.paste then Some Paste_request
    else if hit m.keymap.word_backward then Some (Key_action Word_backward)
    else if hit m.keymap.word_forward then Some (Key_action Word_forward)
    else if hit m.keymap.input_begin then Some (Key_action Input_begin)
    else if hit m.keymap.input_end then Some (Key_action Input_end)
    else if hit m.keymap.uppercase_word_forward then
      Some (Key_action Uppercase_word_forward)
    else if hit m.keymap.lowercase_word_forward then
      Some (Key_action Lowercase_word_forward)
    else if hit m.keymap.capitalize_word_forward then
      Some (Key_action Capitalize_word_forward)
    else if hit m.keymap.transpose_character_backward then
      Some (Key_action Transpose_character_backward)
    else if hit m.keymap.select_character_forward then
      Some (Key_action Select_character_forward)
    else if hit m.keymap.select_character_backward then
      Some (Key_action Select_character_backward)
    else if hit m.keymap.select_word_forward then Some (Key_action Select_word_forward)
    else if hit m.keymap.select_word_backward then Some (Key_action Select_word_backward)
    else if hit m.keymap.select_line_up then Some (Key_action Select_line_up)
    else if hit m.keymap.select_line_down then Some (Key_action Select_line_down)
    else if hit m.keymap.select_all then Some (Key_action Select_all)
    else if hit m.keymap.copy_selection then Some (Copy_request (selected_text m))
    else if
      key.Key.mods.Key.ctrl || key.Key.mods.Key.alt || key.Key.mods.Key.meta
      || key.Key.mods.Key.super || key.Key.mods.Key.hyper
    then None
    else
      let text =
        if key.Key.text <> "" then key.Key.text
        else
          match key.Key.code with
          | Key.Char u ->
              let b = Buffer.create 4 in
              Buffer.add_utf_8_uchar b u;
              Buffer.contents b
          | Key.Space -> " "
          | _ -> ""
      in
      if text = "" then None else Some (Insert text)

let subscriptions m =
  if m.virtual_cursor && m.focused then
    Sub.map (fun msg -> Cursor msg) (Cursor.subscriptions m.cursor)
  else Sub.none

let focus m =
  let m = rebuild { m with focused = true } in
  ({ m with cursor = Cursor.show m.cursor }, Cmd.none)

let blur m = rebuild { m with focused = false }

let cursor m =
  if m.virtual_cursor || not m.focused then None
  else
    let info = line_info m in
    let visual_row, _, _, _ = visual_location m in
    let st = m.styles.cursor in
    Some
      {
        Charamel_tea.Cursor.row = max 0 (visual_row - Viewport.y_offset m.viewport);
        col = gutter_width m + info.char_offset;
        shape = st.shape;
        blink = st.blink;
      }

let set_prompt prompt m = rebuild { m with prompt; prompt_func = None }
let set_prompt_func ~width fn m = rebuild { m with prompt_func = Some (max 0 width, fn) }
let set_placeholder placeholder m = rebuild { m with placeholder }
let set_show_line_numbers show_line_numbers m = rebuild { m with show_line_numbers }

let set_end_of_buffer_character end_of_buffer_character m =
  rebuild { m with end_of_buffer_character }

let set_char_limit limit m =
  let char_limit = max 0 limit in
  let atoms = atoms_of_lines m.lines in
  let atoms = if char_limit > 0 then take_atoms_units char_limit 0 [] atoms else atoms in
  let lines = lines_of_atoms atoms in
  let old_index = position_index m.lines { row = m.row; col = m.col } in
  let p = index_position lines (min old_index (Stdlib.List.length atoms)) in
  rebuild { m with char_limit; lines; row = p.row; col = p.col }

let set_max_height max_height m =
  let max_height = clamp max_height 1 max_lines in
  rebuild
    {
      m with
      max_height;
      min_height = min m.min_height max_height;
      height = min m.height max_height;
    }

let set_max_width max_width m = rebuild { m with max_width = max 1 max_width }
let set_dynamic_height dynamic_height m = rebuild { m with dynamic_height }

let set_min_height min_height m =
  let min_height = clamp min_height 1 m.max_height in
  rebuild { m with min_height; height = max min_height m.height }

let set_max_content_height max_content_height m =
  rebuild { m with max_content_height = max 0 max_content_height }

let set_keymap keymap m = { m with keymap }
let set_styles styles m = rebuild { m with styles }
let set_virtual_cursor virtual_cursor m = rebuild { m with virtual_cursor }
let set_width width m = rebuild { m with width = max 0 width }

let set_height height m =
  rebuild { m with height = clamp height m.min_height m.max_height }

let scroll_y_offset m = Viewport.y_offset m.viewport
let scroll_percent m = Viewport.scroll_percent m.viewport

let position_at ~x ~y m =
  let rows = visual_lines m in
  let visual =
    clamp (Viewport.y_offset m.viewport + y) 0 (max 0 (Stdlib.List.length rows - 1))
  in
  match visual_at rows visual with
  | None -> { row = 0; col = 0 }
  | Some v ->
      let target = max 0 (x - gutter_width m) in
      let rec find col used = function
        | [] -> col
        | cluster :: rest ->
            if used + cluster_width cluster > target then col
            else find (col + 1) (used + cluster_width cluster) rest
      in
      {
        row = v.logical;
        col =
          min
            (Stdlib.List.length (Stdlib.List.nth m.lines v.logical))
            (v.start + find 0 0 v.actual);
      }

let begin_selection ~x ~y m =
  let p = position_at ~x ~y m in
  rebuild (select_from p p { m with selecting = true })

let extend_selection ~x ~y m =
  match (m.selection_anchor, m.selecting) with
  | Some anchor, true ->
      let p = position_at ~x ~y m in
      rebuild (select_from anchor p m)
  | _ -> m

let end_selection m =
  let m = if has_selection m then { m with selecting = false } else clear_selection m in
  rebuild m

let select_all m =
  rebuild
    (select_from { row = 0; col = 0 }
       {
         row = line_count m - 1;
         col = Stdlib.List.length (Stdlib.List.nth m.lines (line_count m - 1));
       }
       m)
