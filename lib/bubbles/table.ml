module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module Style = Charamel_lipgloss.Style
module Text = Charamel_ansi.Text
module Layout = Charamel_lipgloss.Layout

type column = { title : string; width : int }
type row = string list

type keymap = {
  line_up : Key_binding.t;
  line_down : Key_binding.t;
  page_up : Key_binding.t;
  page_down : Key_binding.t;
  half_page_up : Key_binding.t;
  half_page_down : Key_binding.t;
  goto_top : Key_binding.t;
  goto_bottom : Key_binding.t;
}

let default_keymap =
  {
    line_up = Key_binding.v ~help:("↑/k", "up") [ "up"; "k" ];
    line_down = Key_binding.v ~help:("↓/j", "down") [ "down"; "j" ];
    page_up = Key_binding.v ~help:("b/pgup", "page up") [ "b"; "pgup" ];
    page_down = Key_binding.v ~help:("f/pgdn", "page down") [ "f"; "pgdown"; "space" ];
    half_page_up = Key_binding.v ~help:("u", "½ page up") [ "u"; "ctrl+u" ];
    half_page_down = Key_binding.v ~help:("d", "½ page down") [ "d"; "ctrl+d" ];
    goto_top = Key_binding.v ~help:("g/home", "go to start") [ "home"; "g" ];
    goto_bottom = Key_binding.v ~help:("G/end", "go to end") [ "end"; "G" ];
  }

type styles = { header : Style.t; cell : Style.t; selected : Style.t }

let default_styles =
  {
    selected =
      Style.foreground (Charamel_ansi.Color.Indexed 212) (Style.bold true Style.empty);
    header =
      Style.padding
        (Charamel_lipgloss.Sides.v ~right:1 ~left:1 ())
        (Style.bold true Style.empty);
    cell = Style.padding (Charamel_lipgloss.Sides.v ~right:1 ~left:1 ()) Style.empty;
  }

type msg =
  | Line_up
  | Line_down
  | Page_up
  | Page_down
  | Half_page_up
  | Half_page_down
  | Goto_top
  | Goto_bottom

type t = {
  columns : column list;
  rows : row list;
  width : int;
  outer_height : int;
  focused : bool;
  styles : styles;
  keymap : keymap;
  viewport : Viewport.t;
  help : Help.t;
  cursor : int;
  start : int;
  stop : int;
}

let headers_view m =
  let cells =
    Stdlib.List.filter_map
      (fun (column : column) ->
        if column.width <= 0 then None
        else
          let style =
            Style.inline true
              (Style.max_width column.width (Style.width column.width Style.empty))
          in
          let value = Text.truncate ~tail:"…" ~width:column.width column.title in
          Some (Style.render m.styles.header (Style.render style value)))
      m.columns
  in
  Layout.join_horizontal cells

let render_row m row_index row =
  let cells =
    Stdlib.List.mapi
      (fun i (column : column) ->
        if column.width <= 0 then None
        else
          let value =
            match Stdlib.List.nth_opt row i with Some value -> value | None -> ""
          in
          let style =
            Style.inline true
              (Style.max_width column.width (Style.width column.width Style.empty))
          in
          let value = Text.truncate ~tail:"…" ~width:column.width value in
          Some (Style.render m.styles.cell (Style.render style value)))
      m.columns
    |> Stdlib.List.filter_map (fun value -> value)
  in
  let line = Layout.join_horizontal cells in
  if row_index = m.cursor then Style.render m.styles.selected line else line

let update_viewport m =
  let length = Stdlib.List.length m.rows in
  let viewport_height = Viewport.height m.viewport in
  let start = Range.clamp 0 m.cursor (m.cursor - viewport_height) in
  let stop = Range.clamp m.cursor length (m.cursor + viewport_height) in
  let visible =
    Stdlib.List.filteri (fun i _ -> i >= start && i < stop) m.rows
    |> Stdlib.List.mapi (fun i row -> render_row m (start + i) row)
  in
  let content = String.concat "\n" visible in
  let viewport = Viewport.set_content content m.viewport in
  let viewport =
    Viewport.set_y_offset
      (Range.clamp 0
         (max 0 (Viewport.total_line_count viewport - viewport_height))
         (m.cursor - start))
      viewport
  in
  { m with viewport; start; stop }

let v ?(columns = []) ?(rows = []) ?(height = 20) ?(width = 0) ?(focused = false)
    ?(styles = default_styles) ?(keymap = default_keymap) () =
  let header_lines = if columns = [] then 0 else 1 in
  let viewport =
    Viewport.v ~width:(max 0 width) ~height:(max 0 (height - header_lines)) ()
  in
  let help = Help.v ~width () in
  let cursor = 0 in
  update_viewport
    {
      columns;
      rows;
      width = max 0 width;
      outer_height = max 0 height;
      focused;
      styles;
      keymap;
      viewport;
      help;
      cursor;
      start = 0;
      stop = 0;
    }

let rows m = m.rows
let columns m = m.columns
let selected_row m = Stdlib.List.nth_opt m.rows m.cursor
let cursor m = m.cursor

let view_cursor m =
  if not m.focused then None
  else
    match Stdlib.List.nth_opt m.rows m.cursor with
    | None -> None
    | Some _ ->
        let header_lines = if m.columns = [] then 0 else 1 in
        Some
          (Charamel_tea.Cursor.v ~blink:false
             (header_lines + (m.cursor - m.start) - Viewport.y_offset m.viewport)
             0)

let focused m = m.focused
let width m = m.width
let height m = Viewport.height m.viewport
let styles m = m.styles

let set_rows rows m =
  let cursor = Range.clamp 0 (max 0 (Stdlib.List.length rows - 1)) m.cursor in
  update_viewport { m with rows; cursor }

let set_columns columns m =
  let header_lines = if columns = [] then 0 else 1 in
  let viewport = Viewport.set_height (max 0 (m.outer_height - header_lines)) m.viewport in
  update_viewport { m with columns; viewport }

let set_width width m =
  let width = max 0 width in
  update_viewport
    {
      m with
      width;
      viewport = Viewport.set_width width m.viewport;
      help = Help.set_width width m.help;
    }

let set_height height m =
  let outer_height = max 0 height in
  let header_lines = if m.columns = [] then 0 else 1 in
  let viewport = Viewport.set_height (max 0 (outer_height - header_lines)) m.viewport in
  update_viewport { m with outer_height; viewport }

let set_cursor n m =
  update_viewport
    { m with cursor = Range.clamp 0 (max 0 (Stdlib.List.length m.rows - 1)) n }

let move_up n m =
  let n = max 0 n in
  let old_offset = Viewport.y_offset m.viewport in
  let old_start = m.start in
  let viewport_height = Viewport.height m.viewport in
  let cursor = Range.clamp 0 (max 0 (Stdlib.List.length m.rows - 1)) (m.cursor - n) in
  let m = update_viewport { m with cursor } in
  let offset =
    if old_start = 0 then Range.clamp 0 cursor old_offset
    else if old_start < viewport_height then
      Range.clamp 0 viewport_height (Range.clamp 0 cursor (old_offset + n))
    else if old_offset >= 1 then Range.clamp 1 viewport_height (old_offset + n)
    else old_offset
  in
  { m with viewport = Viewport.set_y_offset offset m.viewport }

let move_down n m =
  let n = max 0 n in
  let old_offset = Viewport.y_offset m.viewport in
  let old_start = m.start in
  let old_stop = m.stop in
  let viewport_height = Viewport.height m.viewport in
  let rows_length = Stdlib.List.length m.rows in
  let cursor = Range.clamp 0 (max 0 (rows_length - 1)) (m.cursor + n) in
  let m = update_viewport { m with cursor } in
  let offset =
    if old_stop = rows_length && old_offset > 0 then
      Range.clamp 1 viewport_height (old_offset - n)
    else if cursor > (old_stop - old_start) / 2 && old_offset > 0 then
      Range.clamp 1 cursor (old_offset - n)
    else if old_offset > 1 then old_offset
    else if cursor > old_offset + viewport_height - 1 then Range.clamp 0 1 (old_offset + 1)
    else old_offset
  in
  { m with viewport = Viewport.set_y_offset offset m.viewport }

let goto_top m = set_cursor 0 m
let goto_bottom m = set_cursor (Stdlib.List.length m.rows - 1) m
let focus m = update_viewport { m with focused = true }
let blur m = update_viewport { m with focused = false }
let set_styles styles m = update_viewport { m with styles }
let short_help m = [ m.keymap.line_up; m.keymap.line_down ]

let full_help m =
  [
    [ m.keymap.line_up; m.keymap.line_down; m.keymap.goto_top; m.keymap.goto_bottom ];
    [
      m.keymap.page_up; m.keymap.page_down; m.keymap.half_page_up; m.keymap.half_page_down;
    ];
  ]

let help_view m =
  Help.view m.help { Help.short_help = short_help m; full_help = full_help m }

let update message m =
  if not m.focused then (m, Cmd.none)
  else
    let m =
      match message with
      | Line_up -> move_up 1 m
      | Line_down -> move_down 1 m
      | Page_up -> move_up (Viewport.height m.viewport) m
      | Page_down -> move_down (Viewport.height m.viewport) m
      | Half_page_up -> move_up (Viewport.height m.viewport / 2) m
      | Half_page_down -> move_down (Viewport.height m.viewport / 2) m
      | Goto_top -> goto_top m
      | Goto_bottom -> goto_bottom m
    in
    (m, Cmd.none)

let key m key =
  if not m.focused then None
  else if Key_binding.matches key m.keymap.line_up then Some Line_up
  else if Key_binding.matches key m.keymap.line_down then Some Line_down
  else if Key_binding.matches key m.keymap.page_up then Some Page_up
  else if Key_binding.matches key m.keymap.page_down then Some Page_down
  else if Key_binding.matches key m.keymap.half_page_up then Some Half_page_up
  else if Key_binding.matches key m.keymap.half_page_down then Some Half_page_down
  else if Key_binding.matches key m.keymap.goto_top then Some Goto_top
  else if Key_binding.matches key m.keymap.goto_bottom then Some Goto_bottom
  else None

let subscriptions _ = Sub.none

let view m =
  let headers = headers_view m in
  let body = Viewport.view m.viewport in
  if headers = "" then body else headers ^ "\n" ^ body

let split_line ~separator line =
  let separator_length = String.length separator in
  if separator_length = 0 then [ line ]
  else
    let length = String.length line in
    let rec loop position field_start acc =
      if
        position + separator_length <= length
        && String.sub line position separator_length = separator
      then
        let field = String.sub line field_start (position - field_start) in
        loop (position + separator_length) (position + separator_length) (field :: acc)
      else if position >= length then
        Stdlib.List.rev (String.sub line field_start (length - field_start) :: acc)
      else loop (position + 1) field_start acc
    in
    loop 0 0 []

let of_values ?(separator = ",") value =
  String.split_on_char '\n' value |> Stdlib.List.map (split_line ~separator)
