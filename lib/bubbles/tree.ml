module Cmd = Charm_tea.Cmd
module Sub = Charm_tea.Sub
module Style = Charm_lipgloss.Style
module Color = Charm_ansi.Color
module Text = Charm_ansi.Text
module Sides = Charm_lipgloss.Sides

let clamp n lo hi = max lo (min hi n)
let key_binding ?help names = Key_binding.v ?help names

type node = { open_ : bool; value : string; children : node list }

let node ?(open_ = true) ?(value = "") children = { open_; value; children }
let leaf value = { open_ = false; value; children = [] }
let value n = n.value
let children n = n.children
let is_open n = n.open_

let rec size n =
  1
  +
  if n.open_ then
    Stdlib.List.fold_left (fun count child -> count + size child) 0 n.children
  else 0

type keymap = {
  down : Key_binding.t;
  up : Key_binding.t;
  page_down : Key_binding.t;
  page_up : Key_binding.t;
  half_page_down : Key_binding.t;
  half_page_up : Key_binding.t;
  go_to_top : Key_binding.t;
  go_to_bottom : Key_binding.t;
  toggle : Key_binding.t;
  open_ : Key_binding.t;
  close : Key_binding.t;
  show_full_help : Key_binding.t;
  close_full_help : Key_binding.t;
}

type styles = {
  tree_style : Style.t;
  help_style : Style.t;
  node_style : Style.t;
  selected_node_style : Style.t;
  root_node_style : Style.t;
  parent_node_style : Style.t;
  cursor_style : Style.t;
  enumerator_style : Style.t;
  selected_enumerator_style : Style.t;
  indenter_style : Style.t;
  open_indicator_style : Style.t;
}

let default_keymap =
  {
    down = key_binding ~help:("↓/j", "down") [ "down"; "j"; "ctrl+n" ];
    up = key_binding ~help:("↑/k", "up") [ "up"; "k"; "ctrl+p" ];
    page_down = key_binding ~help:("f/pgdn", "page down") [ "pgdown"; "space"; "f" ];
    page_up = key_binding ~help:("b/pgup", "page up") [ "pgup"; "b" ];
    half_page_down = key_binding ~help:("d", "½ page down") [ "d"; "ctrl+d" ];
    half_page_up = key_binding ~help:("u", "½ page up") [ "u"; "ctrl+u" ];
    go_to_top = key_binding ~help:("g", "top") [ "g"; "home" ];
    go_to_bottom = key_binding ~help:("G", "bottom") [ "G"; "end" ];
    toggle = key_binding ~help:("⏎", "toggle") [ "enter" ];
    open_ = key_binding ~help:("→/l", "open") [ "l"; "right" ];
    close = key_binding ~help:("←/h", "close") [ "h"; "left" ];
    show_full_help = key_binding ~help:("?", "more") [ "?" ];
    close_full_help = key_binding ~help:("?", "close help") [ "?" ];
  }

let color_hex fallback text =
  match Color.of_hex text with Some color -> color | None -> fallback

let default_styles ~is_dark =
  let light_dark light dark = Charm_lipgloss.light_dark ~is_dark ~light ~dark in
  let subdued =
    light_dark
      (color_hex (Color.Indexed 245) "#9B9B9B")
      (color_hex (Color.Indexed 245) "#5C5C5C")
  in
  let very_subdued =
    light_dark
      (color_hex (Color.Indexed 245) "#DDDADA")
      (color_hex (Color.Indexed 245) "#3C3C3C")
  in
  {
    tree_style = Style.empty;
    help_style = Style.padding (Sides.v ~top:1 ()) Style.empty;
    node_style =
      Style.foreground
        (light_dark
           (color_hex (Color.Indexed 245) "#9B9B9B")
           (color_hex (Color.Indexed 245) "#B0B0B0"))
        Style.empty;
    selected_node_style =
      Style.bold true
        (Style.foreground
           (light_dark (Color.Indexed 249) (Color.Indexed 212))
           Style.empty);
    root_node_style =
      Style.foreground (color_hex (Color.Indexed 212) "#EE6FF8") Style.empty;
    parent_node_style =
      Style.foreground (light_dark (Color.Indexed 90) (Color.Indexed 99)) Style.empty;
    cursor_style =
      Style.bold true
        (Style.padding (Sides.v ~right:1 ())
           (Style.foreground
              (light_dark (Color.Indexed 249) (Color.Indexed 212))
              Style.empty));
    enumerator_style = Style.foreground very_subdued Style.empty;
    selected_enumerator_style = Style.foreground very_subdued Style.empty;
    indenter_style = Style.foreground very_subdued Style.empty;
    open_indicator_style = Style.foreground subdued Style.empty;
  }

type msg =
  | Down
  | Up
  | Page_down
  | Page_up
  | Half_page_down
  | Half_page_up
  | Go_to_top
  | Go_to_bottom
  | Toggle
  | Open
  | Close
  | Toggle_full_help

type t = {
  root : node;
  open_character : string;
  closed_character : string;
  cursor_character : string;
  scroll_off : int;
  show_help : bool;
  show_full_help : bool;
  keymap : keymap;
  styles : styles;
  width : int;
  height : int;
  y_offset : int;
  additional_short_help_keys : Key_binding.t list;
  additional_full_help_keys : Key_binding.t list;
}

let v ?(open_character = "▼") ?(closed_character = "▶") ?(cursor_character = "→")
    ?(scroll_off = 5) ?(show_help = true) ?(keymap = default_keymap) ?(is_dark = true)
    ?styles ~width ~height root =
  let styles =
    match styles with Some styles -> styles | None -> default_styles ~is_dark
  in
  {
    root;
    open_character;
    closed_character;
    cursor_character;
    scroll_off = max 0 scroll_off;
    show_help;
    show_full_help = false;
    keymap;
    styles;
    width = max 0 width;
    height = max 0 height;
    y_offset = 0;
    additional_short_help_keys = [];
    additional_full_help_keys = [];
  }

type located = { node : node; path : int list; depth : int; prefix : string }

let rec visible_nodes ?(path = []) ?(depth = 0) ?(spine = "") ?(last = true) node =
  let branch = if depth = 0 then "" else if last then "└──" else "├──" in
  let prefix = spine ^ branch in
  let self = { node; path; depth; prefix } in
  if not (node : node).open_ then [ self ]
  else
    let count = Stdlib.List.length node.children in
    let children_spine =
      if depth = 0 then spine else spine ^ if last then "   " else "│  "
    in
    let children =
      Stdlib.List.mapi
        (fun index child ->
          visible_nodes ~path:(path @ [ index ]) ~depth:(depth + 1) ~spine:children_spine
            ~last:(index = count - 1)
            child)
        node.children
      |> Stdlib.List.concat
    in
    self :: children

let all_nodes t = Stdlib.List.map (fun item -> item.node) (visible_nodes t.root)

let node_at_current_offset t =
  let nodes = all_nodes t in
  Stdlib.List.nth_opt nodes t.y_offset

let rec replace_at_path path f node =
  match path with
  | [] -> f node
  | index :: rest ->
      let children =
        Stdlib.List.mapi
          (fun child_index child ->
            if child_index = index then replace_at_path rest f child else child)
          node.children
      in
      { node with children }

let update_current_node f t =
  match Stdlib.List.nth_opt (visible_nodes t.root) t.y_offset with
  | None -> t
  | Some item ->
      let root = replace_at_path item.path f t.root in
      { t with root; y_offset = clamp t.y_offset 0 (max 0 (size root - 1)) }

let selected_index t = clamp t.y_offset 0 (max 0 (Stdlib.List.length (all_nodes t) - 1))

let set_y_offset offset t =
  { t with y_offset = selected_index { t with y_offset = offset } }

let y_offset t = t.y_offset
let root t = t.root
let width t = t.width
let height t = t.height
let set_width width t = { t with width = max 0 width }
let set_height height t = { t with height = max 0 height }
let set_size ~width ~height t = { t with width = max 0 width; height = max 0 height }

let set_root root t =
  { t with root; y_offset = clamp t.y_offset 0 (max 0 (size root - 1)) }

let set_open_character open_character t = { t with open_character }
let set_closed_character closed_character t = { t with closed_character }
let set_cursor_character cursor_character t = { t with cursor_character }
let set_scroll_off scroll_off t = { t with scroll_off = max 0 scroll_off }
let set_show_help show_help t = { t with show_help }
let set_styles styles t = { t with styles }
let down t = set_y_offset (t.y_offset + 1) t
let up t = set_y_offset (t.y_offset - 1) t
let page_size t = max 1 (t.height - if t.show_help then 2 else 0)
let page_down t = set_y_offset (t.y_offset + page_size t) t
let page_up t = set_y_offset (t.y_offset - page_size t) t
let half_page_down t = set_y_offset (t.y_offset + max 1 (page_size t / 2)) t
let half_page_up t = set_y_offset (t.y_offset - max 1 (page_size t / 2)) t
let go_to_top t = set_y_offset 0 t
let go_to_bottom t = set_y_offset (max 0 (Stdlib.List.length (all_nodes t) - 1)) t

let toggle_current_node t =
  update_current_node
    (fun (n : node) -> if n.children = [] then n else { n with open_ = not n.open_ })
    t

let open_current_node t =
  update_current_node
    (fun (n : node) -> if n.children = [] then n else { n with open_ = true })
    t

let close_current_node t =
  update_current_node (fun (n : node) -> { n with open_ = false }) t

let action_for_key t key =
  if Key_binding.matches key t.keymap.down then Some Down
  else if Key_binding.matches key t.keymap.up then Some Up
  else if Key_binding.matches key t.keymap.page_down then Some Page_down
  else if Key_binding.matches key t.keymap.page_up then Some Page_up
  else if Key_binding.matches key t.keymap.half_page_down then Some Half_page_down
  else if Key_binding.matches key t.keymap.half_page_up then Some Half_page_up
  else if Key_binding.matches key t.keymap.go_to_top then Some Go_to_top
  else if Key_binding.matches key t.keymap.go_to_bottom then Some Go_to_bottom
  else if Key_binding.matches key t.keymap.toggle then Some Toggle
  else if Key_binding.matches key t.keymap.open_ then Some Open
  else if Key_binding.matches key t.keymap.close then Some Close
  else if Key_binding.matches key t.keymap.show_full_help then Some Toggle_full_help
  else if Key_binding.matches key t.keymap.close_full_help then Some Toggle_full_help
  else None

let key t key = action_for_key t key
let subscriptions _ = Sub.none

let update message t =
  let model =
    match message with
    | Down -> down t
    | Up -> up t
    | Page_down -> page_down t
    | Page_up -> page_up t
    | Half_page_down -> half_page_down t
    | Half_page_up -> half_page_up t
    | Go_to_top -> go_to_top t
    | Go_to_bottom -> go_to_bottom t
    | Toggle -> toggle_current_node t
    | Open -> open_current_node t
    | Close -> close_current_node t
    | Toggle_full_help -> { t with show_full_help = not t.show_full_help }
  in
  (model, Cmd.none)

let set_additional_short_help_keys keys t = { t with additional_short_help_keys = keys }
let set_additional_full_help_keys keys t = { t with additional_full_help_keys = keys }

let short_help t =
  [ t.keymap.down; t.keymap.up; t.keymap.toggle ]
  @ t.additional_short_help_keys @ [ t.keymap.show_full_help ]

let full_help t =
  [
    [ t.keymap.down; t.keymap.up; t.keymap.open_; t.keymap.close; t.keymap.toggle ];
    [
      t.keymap.page_down; t.keymap.page_up; t.keymap.half_page_down; t.keymap.half_page_up;
    ];
    [ t.keymap.go_to_top; t.keymap.go_to_bottom ];
  ]
  @ (if t.additional_full_help_keys = [] then [] else [ t.additional_full_help_keys ])
  @ [ [ t.keymap.close_full_help ] ]

let node_style t item selected =
  if selected then t.styles.selected_node_style
  else if item.depth = 0 then t.styles.root_node_style
  else if item.node.children <> [] then t.styles.parent_node_style
  else t.styles.node_style

let indicator t item selected =
  if item.node.children = [] then ""
  else
    let text = if item.node.open_ then t.open_character else t.closed_character in
    let style =
      if selected then t.styles.selected_enumerator_style
      else t.styles.open_indicator_style
    in
    Style.render style (text ^ " ")

let line_parts t item selected =
  let values =
    match String.split_on_char '\n' item.node.value with [] -> [ "" ] | xs -> xs
  in
  let base_prefix = if item.depth = 0 then "" else item.prefix in
  let continuation =
    if item.depth = 0 then "" else String.make (Text.width base_prefix) ' '
  in
  Stdlib.List.mapi
    (fun line_index text ->
      let cursor =
        if selected && line_index = 0 then
          Style.render t.styles.cursor_style t.cursor_character
        else Style.render t.styles.cursor_style " "
      in
      let prefix = if line_index = 0 then base_prefix else continuation in
      let prefix_style =
        if item.depth = 0 then t.styles.indenter_style else t.styles.enumerator_style
      in
      let prefix = Style.render prefix_style prefix in
      let value = Style.render (node_style t item selected) text in
      cursor ^ prefix ^ indicator t item selected ^ value)
    values

let rendered_lines t =
  let items = visible_nodes t.root in
  let rec build index items acc =
    match items with
    | [] -> Stdlib.List.rev acc
    | item :: rest ->
        build (index + 1) rest
          (Stdlib.List.rev_append (line_parts t item (index = t.y_offset)) acc)
  in
  build 0 items []

let selected_line_offset t =
  let items = visible_nodes t.root in
  let rec count index lines =
    if index >= t.y_offset then lines
    else
      match Stdlib.List.nth_opt items index with
      | None -> lines
      | Some item ->
          let line_count =
            1
            + String.fold_left
                (fun n c -> if c = '\n' then n + 1 else n)
                0 item.node.value
          in
          count (index + 1) (lines + line_count)
  in
  count 0 0

let tree_content t =
  let lines = rendered_lines t in
  let content = String.concat "\n" lines in
  let viewport_height = max 0 (t.height - if t.show_help then 2 else 0) in
  let viewport =
    Viewport.v ~width:t.width ~height:viewport_height () |> Viewport.set_content content
  in
  let scroll_off = min t.scroll_off (max 0 (viewport_height / 2)) in
  let offset = max 0 (selected_line_offset t - scroll_off) in
  Viewport.set_y_offset offset viewport |> Viewport.view

let help_content t =
  let help = Help.v ~width:t.width ~show_all:t.show_full_help () in
  Help.view help { Help.short_help = short_help t; full_help = full_help t }

let view t =
  let content = Style.render t.styles.tree_style (tree_content t) in
  if not t.show_help then content
  else
    Charm_lipgloss.Layout.join_vertical
      [ content; Style.render t.styles.help_style (help_content t) ]
