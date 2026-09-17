module Cmd = Charm_tea.Cmd
module Sub = Charm_tea.Sub
module Style = Charm_lipgloss.Style
module Layout = Charm_lipgloss.Layout
module Text = Charm_ansi.Text
module Color = Charm_ansi.Color

let clamp n lo hi =
  let lo, hi = if lo <= hi then (lo, hi) else (hi, lo) in
  max lo (min hi n)

let color_hex value =
  match Color.of_hex value with Some color -> color | None -> Color.Default

let light_dark ~is_dark ~light ~dark =
  Charm_lipgloss.light_dark ~is_dark ~light:(color_hex light) ~dark:(color_hex dark)

type filter_state = Unfiltered | Filtering | Filter_applied
type rank = { index : int; matched : int list }
type filter = string -> string list -> rank list

let default_filter term targets =
  Stdlib.List.map
    (fun (m : Fuzzy.match_) -> { index = m.Fuzzy.index; matched = m.Fuzzy.matched })
    (Fuzzy.find ~pattern:term targets)

let unsorted_filter term targets =
  Stdlib.List.map
    (fun (m : Fuzzy.match_) -> { index = m.Fuzzy.index; matched = m.Fuzzy.matched })
    (Fuzzy.find_unsorted ~pattern:term targets)

type keymap = {
  cursor_up : Key_binding.t;
  cursor_down : Key_binding.t;
  next_page : Key_binding.t;
  prev_page : Key_binding.t;
  go_to_start : Key_binding.t;
  go_to_end : Key_binding.t;
  filter : Key_binding.t;
  clear_filter : Key_binding.t;
  cancel_while_filtering : Key_binding.t;
  accept_while_filtering : Key_binding.t;
  show_full_help : Key_binding.t;
  close_full_help : Key_binding.t;
  quit : Key_binding.t;
  force_quit : Key_binding.t;
}

let default_keymap =
  {
    cursor_up = Key_binding.v ~help:("↑/k", "up") [ "up"; "k" ];
    cursor_down = Key_binding.v ~help:("↓/j", "down") [ "down"; "j" ];
    prev_page =
      Key_binding.v ~help:("←/h/pgup", "prev page") [ "left"; "h"; "pgup"; "b"; "u" ];
    next_page =
      Key_binding.v ~help:("→/l/pgdn", "next page") [ "right"; "l"; "pgdown"; "f"; "d" ];
    go_to_start = Key_binding.v ~help:("g/home", "go to start") [ "home"; "g" ];
    go_to_end = Key_binding.v ~help:("G/end", "go to end") [ "end"; "G" ];
    filter = Key_binding.v ~help:("/", "filter") [ "/" ];
    clear_filter = Key_binding.v ~help:("esc", "clear filter") [ "esc" ];
    cancel_while_filtering = Key_binding.v ~help:("esc", "cancel") [ "esc" ];
    accept_while_filtering =
      Key_binding.v ~help:("enter", "apply filter")
        [ "enter"; "tab"; "shift+tab"; "ctrl+k"; "up"; "ctrl+j"; "down" ];
    show_full_help = Key_binding.v ~help:("?", "more") [ "?" ];
    close_full_help = Key_binding.v ~help:("?", "close help") [ "?" ];
    quit = Key_binding.v ~help:("q", "quit") [ "q"; "esc" ];
    force_quit = Key_binding.v [ "ctrl+c" ];
  }

type styles = {
  title_bar : Style.t;
  title : Style.t;
  spinner : Style.t;
  filter_prompt : Style.t;
  filter_cursor : Style.t;
  default_filter_character_match : Style.t;
  status_bar : Style.t;
  status_empty : Style.t;
  status_bar_active_filter : Style.t;
  status_bar_filter_count : Style.t;
  no_items : Style.t;
  pagination_style : Style.t;
  help_style : Style.t;
  active_pagination_dot : Style.t;
  inactive_pagination_dot : Style.t;
  arabic_pagination : Style.t;
  divider_dot : Style.t;
}

let default_styles ~is_dark =
  let subdued = light_dark ~is_dark ~light:"#9B9B9B" ~dark:"#5C5C5C" in
  let very_subdued = light_dark ~is_dark ~light:"#DDDADA" ~dark:"#3C3C3C" in
  let title_bar =
    Style.padding (Charm_lipgloss.Sides.v ~bottom:1 ~left:2 ()) Style.empty
  in
  {
    title_bar;
    title =
      Style.padding
        (Charm_lipgloss.Sides.v ~right:1 ~left:1 ())
        (Style.foreground (Color.Indexed 230)
           (Style.background (Color.Indexed 62) Style.empty));
    spinner =
      Style.foreground (light_dark ~is_dark ~light:"#8E8E8E" ~dark:"#747373") Style.empty;
    filter_prompt =
      Style.foreground (light_dark ~is_dark ~light:"#04B575" ~dark:"#ECFD65") Style.empty;
    filter_cursor = Style.foreground (color_hex "#EE6FF8") Style.empty;
    default_filter_character_match = Style.underline true Style.empty;
    status_bar =
      Style.padding
        (Charm_lipgloss.Sides.v ~bottom:1 ~left:2 ())
        (Style.foreground
           (light_dark ~is_dark ~light:"#A49FA5" ~dark:"#777777")
           Style.empty);
    status_empty = Style.foreground subdued Style.empty;
    status_bar_active_filter =
      Style.foreground (light_dark ~is_dark ~light:"#1a1a1a" ~dark:"#dddddd") Style.empty;
    status_bar_filter_count = Style.foreground very_subdued Style.empty;
    no_items =
      Style.foreground (light_dark ~is_dark ~light:"#909090" ~dark:"#626262") Style.empty;
    pagination_style = Style.padding (Charm_lipgloss.Sides.v ~left:2 ()) Style.empty;
    help_style = Style.padding (Charm_lipgloss.Sides.v ~top:1 ~left:2 ()) Style.empty;
    active_pagination_dot =
      Style.foreground (light_dark ~is_dark ~light:"#847A85" ~dark:"#979797") Style.empty;
    inactive_pagination_dot = Style.foreground very_subdued Style.empty;
    arabic_pagination = Style.foreground subdued Style.empty;
    divider_dot = Style.foreground very_subdued Style.empty;
  }

type 'a item_context = {
  index : int;
  selected : bool;
  filter_state : filter_state;
  filter_text : string;
  matched : int list;
  width : int;
}

type 'a delegate = {
  height : int;
  spacing : int;
  render : 'a item_context -> 'a -> string;
  short_help : Key_binding.t list;
  full_help : Key_binding.t list list;
}

type item_styles = {
  normal_title : Style.t;
  normal_desc : Style.t;
  selected_title : Style.t;
  selected_desc : Style.t;
  dimmed_title : Style.t;
  dimmed_desc : Style.t;
  filter_match : Style.t;
}

let default_item_styles ~is_dark =
  let normal_title =
    Style.padding
      (Charm_lipgloss.Sides.v ~left:2 ())
      (Style.foreground
         (light_dark ~is_dark ~light:"#1a1a1a" ~dark:"#dddddd")
         Style.empty)
  in
  let normal_desc =
    Style.foreground (light_dark ~is_dark ~light:"#A49FA5" ~dark:"#777777") normal_title
  in
  let selected_title =
    Style.padding
      (Charm_lipgloss.Sides.v ~left:1 ())
      (Style.foreground (color_hex "#EE6FF8")
         (Style.border_left true
            (Style.border_foreground
               (Charm_lipgloss.Sides_color.v ~left:(color_hex "#F793FF") ())
               (Style.border Charm_lipgloss.Border.normal Style.empty))))
  in
  let selected_desc =
    Style.foreground (light_dark ~is_dark ~light:"#F793FF" ~dark:"#AD58B4") selected_title
  in
  let dimmed_title =
    Style.padding
      (Charm_lipgloss.Sides.v ~left:2 ())
      (Style.foreground
         (light_dark ~is_dark ~light:"#A49FA5" ~dark:"#777777")
         Style.empty)
  in
  let dimmed_desc =
    Style.foreground (light_dark ~is_dark ~light:"#C2B8C2" ~dark:"#4D4D4D") dimmed_title
  in
  {
    normal_title;
    normal_desc;
    selected_title;
    selected_desc;
    dimmed_title;
    dimmed_desc;
    filter_match = Style.underline true Style.empty;
  }

let style_padding_width style =
  match Style.get_padding style with
  | Some p -> p.Charm_lipgloss.Sides.left + p.Charm_lipgloss.Sides.right
  | None -> 0

let default_delegate ?(show_description = true) ?(height = 2) ?(spacing = 1) ?styles
    ?(is_dark = true) ~title ?description () =
  let styles = match styles with Some s -> s | None -> default_item_styles ~is_dark in
  let height = if show_description then max 1 height else 1 in
  let description = match description with Some f -> f | None -> fun _ -> "" in
  let render (ctx : 'a item_context) item =
    let available style = max 0 (ctx.width - style_padding_width style) in
    let title =
      Text.truncate ~tail:"…" ~width:(available styles.normal_title) (title item)
    in
    let desc =
      if show_description then
        Text.truncate ~tail:"…" ~width:(available styles.normal_desc) (description item)
      else ""
    in
    let filtered = ctx.filter_state <> Unfiltered in
    let dimmed = ctx.filter_state = Filtering && ctx.filter_text = "" in
    let title_style, desc_style =
      if dimmed then (styles.dimmed_title, styles.dimmed_desc)
      else if ctx.selected && ctx.filter_state <> Filtering then
        (styles.selected_title, styles.selected_desc)
      else (styles.normal_title, styles.normal_desc)
    in
    let title =
      if filtered && ctx.matched <> [] then
        let matched =
          Style.inherit_ ~parent:styles.filter_match (Style.inline true title_style)
        in
        let unmatched = Style.inline true title_style in
        Layout.style_runes matched unmatched title ~indices:ctx.matched
      else title
    in
    let title = Style.render title_style title in
    if show_description then title ^ "\n" ^ Style.render desc_style desc else title
  in
  { height; spacing = max 0 spacing; render; short_help = []; full_help = [] }

type 'a msg =
  | Cursor_up
  | Cursor_down
  | Next_page
  | Prev_page
  | Go_to_start
  | Go_to_end
  | Start_filter
  | Clear_filter
  | Cancel_filter
  | Accept_filter
  | Toggle_full_help
  | Filter_input of Textinput.msg
  | Status_timeout of int
  | Spinner of Spinner.msg
  | Set_items of 'a list

type 'a filtered_item = { index : int; item : 'a; matched : int list }

type 'a t = {
  title : string;
  width : int;
  height : int;
  keymap : keymap;
  styles : styles;
  filter : filter;
  filtering_enabled : bool;
  show_title : bool;
  show_filter : bool;
  show_status_bar : bool;
  show_pagination : bool;
  show_help : bool;
  infinite_scrolling : bool;
  status_message_lifetime : float;
  item_name_singular : string;
  item_name_plural : string;
  delegate : 'a delegate;
  filter_value_fn : 'a -> string;
  items : 'a list;
  filtered_items : 'a filtered_item list;
  filter_state : filter_state;
  filter_input : Textinput.t;
  spinner : Spinner.t;
  show_spinner : bool;
  paginator : Paginator.t;
  help : Help.t;
  status_message : string;
  status_generation : int;
  cursor : int;
  disable_quit : bool;
  additional_short_help : Key_binding.t list;
  additional_full_help : Key_binding.t list;
}

let input_styles ~is_dark styles =
  let input = Textinput.default_styles ~is_dark in
  let focused = { input.Textinput.focused with prompt = styles.filter_prompt } in
  let blurred = { input.Textinput.blurred with prompt = styles.filter_prompt } in
  let cursor =
    match Style.get_foreground styles.filter_cursor with
    | Some color -> { input.Textinput.cursor with color }
    | None -> input.Textinput.cursor
  in
  ({ focused; blurred; cursor } : Textinput.styles)

let set_input_styles m =
  Textinput.set_styles (input_styles ~is_dark:true m.styles) m.filter_input

let rec help_keymap m =
  { Help.short_help = short_help_impl m; full_help = full_help_impl m }

and short_help_impl m =
  let base =
    match m.filter_state with
    | Filtering -> [ m.keymap.accept_while_filtering; m.keymap.cancel_while_filtering ]
    | Unfiltered -> [ m.keymap.cursor_up; m.keymap.cursor_down; m.keymap.filter ]
    | Filter_applied ->
        [ m.keymap.cursor_up; m.keymap.cursor_down; m.keymap.clear_filter ]
  in
  let base = if m.filter_state = Filtering then base else base @ m.delegate.short_help in
  let base =
    if m.filter_state = Filtering then base else base @ m.additional_short_help
  in
  base @ [ m.keymap.quit; m.keymap.show_full_help ]

and full_help_impl m =
  let nav =
    [
      m.keymap.cursor_up;
      m.keymap.cursor_down;
      m.keymap.next_page;
      m.keymap.prev_page;
      m.keymap.go_to_start;
      m.keymap.go_to_end;
    ]
  in
  let delegate = if m.filter_state = Filtering then [] else m.delegate.full_help in
  let actions =
    if m.filter_state = Filtering then
      [ m.keymap.accept_while_filtering; m.keymap.cancel_while_filtering ]
    else [ m.keymap.filter; m.keymap.clear_filter ] @ m.additional_full_help
  in
  nav :: (delegate @ [ actions; [ m.keymap.quit; m.keymap.close_full_help ] ])

let title_view m =
  if m.filter_state = Filtering && m.show_filter then Textinput.view m.filter_input
  else if not m.show_title then ""
  else
    let title = Style.render m.styles.title m.title in
    let status = if m.status_message = "" then "" else "  " ^ m.status_message in
    let spinner = if m.show_spinner then Spinner.view m.spinner else "" in
    let body = title ^ status in
    let body =
      if spinner = "" then body else body ^ " " ^ Style.render m.styles.spinner spinner
    in
    Style.render m.styles.title_bar body

let status_view m =
  let visible_count =
    if m.filter_state = Unfiltered then Stdlib.List.length m.items
    else Stdlib.List.length m.filtered_items
  in
  let item_name =
    if visible_count = 1 then m.item_name_singular else m.item_name_plural
  in
  let base = Fmt.str "%d %s" visible_count item_name in
  let text =
    match m.filter_state with
    | Filtering -> if visible_count = 0 then "Nothing matched" else base
    | Unfiltered ->
        if Stdlib.List.length m.items = 0 then "No " ^ m.item_name_plural else base
    | Filter_applied ->
        let filter_text =
          Text.truncate ~tail:"…" ~width:10 (Text.strip (Textinput.value m.filter_input))
        in
        let hidden = Stdlib.List.length m.items - visible_count in
        let suffix = if hidden > 0 then Fmt.str " • %d filtered" hidden else "" in
        Fmt.str "“%s” %s%s" filter_text base suffix
  in
  let style = if visible_count = 0 then m.styles.status_empty else m.styles.status_bar in
  Style.render style text

let pagination_view m =
  if Paginator.total_pages m.paginator < 2 then ""
  else
    let value = Paginator.view m.paginator in
    let value =
      if Text.width value > m.width then
        Style.render m.styles.arabic_pagination
          (Paginator.view (Paginator.set_kind Paginator.Arabic m.paginator))
      else value
    in
    Style.render m.styles.pagination_style value

let help_view m = Style.render m.styles.help_style (Help.view m.help (help_keymap m))

let chrome_height m =
  let title =
    if m.show_title || (m.show_filter && m.filtering_enabled) then
      Layout.height (title_view m)
    else 0
  in
  let status = if m.show_status_bar then Layout.height (status_view m) else 0 in
  let pagination = if m.show_pagination then Layout.height (pagination_view m) else 0 in
  let help = if m.show_help then Layout.height (help_view m) else 0 in
  title + status + pagination + help

let update_keymap m =
  let enable binding value = Key_binding.set_enabled value binding in
  let has_items =
    if m.filter_state = Unfiltered then Stdlib.List.length m.items > 0
    else Stdlib.List.length m.filtered_items > 0
  in
  let has_pages = Paginator.total_pages m.paginator > 1 in
  let keymap =
    match m.filter_state with
    | Filtering ->
        {
          m.keymap with
          cursor_up = enable m.keymap.cursor_up false;
          cursor_down = enable m.keymap.cursor_down false;
          next_page = enable m.keymap.next_page false;
          prev_page = enable m.keymap.prev_page false;
          go_to_start = enable m.keymap.go_to_start false;
          go_to_end = enable m.keymap.go_to_end false;
          filter = enable m.keymap.filter false;
          clear_filter = enable m.keymap.clear_filter false;
          cancel_while_filtering = enable m.keymap.cancel_while_filtering true;
          accept_while_filtering =
            enable m.keymap.accept_while_filtering (Textinput.value m.filter_input <> "");
          show_full_help = enable m.keymap.show_full_help false;
          close_full_help = enable m.keymap.close_full_help false;
          quit = enable m.keymap.quit false;
        }
    | Unfiltered | Filter_applied ->
        {
          m.keymap with
          cursor_up = enable m.keymap.cursor_up has_items;
          cursor_down = enable m.keymap.cursor_down has_items;
          next_page = enable m.keymap.next_page (has_items && has_pages);
          prev_page = enable m.keymap.prev_page (has_items && has_pages);
          go_to_start = enable m.keymap.go_to_start has_items;
          go_to_end = enable m.keymap.go_to_end has_items;
          filter = enable m.keymap.filter (m.filtering_enabled && has_items);
          clear_filter = enable m.keymap.clear_filter (m.filter_state = Filter_applied);
          cancel_while_filtering = enable m.keymap.cancel_while_filtering false;
          accept_while_filtering = enable m.keymap.accept_while_filtering false;
          show_full_help = enable m.keymap.show_full_help m.show_help;
          close_full_help = enable m.keymap.close_full_help m.show_help;
          quit = enable m.keymap.quit (not m.disable_quit);
        }
  in
  { m with keymap }

let update_pagination m =
  let available = max 1 (m.height - chrome_height m) in
  let per_page = max 1 (available / max 1 (m.delegate.height + m.delegate.spacing)) in
  let paginator = Paginator.set_per_page per_page m.paginator in
  let count =
    if m.filter_state = Unfiltered then Stdlib.List.length m.items
    else Stdlib.List.length m.filtered_items
  in
  let paginator = Paginator.set_total_pages ~items:count paginator in
  let idx = (Paginator.page paginator * per_page) + m.cursor in
  let page =
    clamp
      (if per_page = 0 then 0 else idx / per_page)
      0
      (max 0 (Paginator.total_pages paginator - 1))
  in
  let cursor = idx mod per_page in
  update_keymap { m with paginator = Paginator.set_page page paginator; cursor }

let recompute_filtered m =
  if m.filter_state = Unfiltered then { m with filtered_items = [] }
  else
    let filter_text = Textinput.value m.filter_input in
    let filtered_items =
      if filter_text = "" then
        Stdlib.List.mapi (fun index item -> { index; item; matched = [] }) m.items
      else
        let targets = Stdlib.List.map m.filter_value_fn m.items in
        let ranks = m.filter filter_text targets in
        Stdlib.List.filter_map
          (fun (r : rank) ->
            Option.map
              (fun item -> { index = r.index; item; matched = r.matched })
              (Stdlib.List.nth_opt m.items r.index))
          ranks
    in
    { m with filtered_items }

let v ?(title = "List") ?(width = 0) ?(height = 0) ?(keymap = default_keymap)
    ?(is_dark = true) ?styles ?(filter = default_filter) ?(filtering_enabled = true)
    ?(show_title = true) ?(show_filter = true) ?(show_status_bar = true)
    ?(show_pagination = true) ?(show_help = true) ?(infinite_scrolling = false)
    ?(status_message_lifetime = 1.0) ?(item_name = ("item", "items")) ~delegate
    ~filter_value items =
  let styles : styles =
    match styles with Some value -> value | None -> default_styles ~is_dark
  in
  let filter_input =
    Textinput.v ~prompt:"Filter: " ~char_limit:64
      ~styles:(input_styles ~is_dark styles)
      ~is_dark ()
  in
  let filter_input, _ = Textinput.focus filter_input in
  let spinner = Spinner.v ~kind:Spinner.Line ~style:styles.spinner () in
  let paginator = Paginator.v ~kind:Paginator.Dots ~active_dot:"•" ~inactive_dot:"•" () in
  let help = Help.v ~width () in
  let singular, plural = item_name in
  let m =
    {
      title;
      width = max 0 width;
      height = max 0 height;
      keymap;
      styles;
      filter;
      filtering_enabled;
      show_title;
      show_filter;
      show_status_bar;
      show_pagination;
      show_help;
      infinite_scrolling;
      status_message_lifetime;
      item_name_singular = singular;
      item_name_plural = plural;
      delegate;
      filter_value_fn = filter_value;
      items;
      filtered_items = [];
      filter_state = Unfiltered;
      filter_input;
      spinner;
      show_spinner = false;
      paginator;
      help;
      status_message = "";
      status_generation = 0;
      cursor = 0;
      disable_quit = false;
      additional_short_help = [];
      additional_full_help = [];
    }
  in
  update_pagination m

let items m = m.items

let visible_items m =
  if m.filter_state = Unfiltered then m.items
  else Stdlib.List.map (fun x -> x.item) m.filtered_items

let index m = (Paginator.page m.paginator * Paginator.per_page m.paginator) + m.cursor
let selected_item m = Stdlib.List.nth_opt (visible_items m) (index m)

let global_index m =
  match m.filter_state with
  | Unfiltered -> index m
  | _ -> (
      match Stdlib.List.nth_opt m.filtered_items (index m) with
      | Some x -> x.index
      | None -> index m)

let set_items items m =
  let m = { m with items } in
  let m = if m.filter_state = Unfiltered then m else recompute_filtered m in
  update_pagination m

let select n m =
  let per_page = max 1 (Paginator.per_page m.paginator) in
  let count = Stdlib.List.length (visible_items m) in
  let n = clamp n 0 (max 0 (count - 1)) in
  update_pagination
    {
      m with
      paginator = Paginator.set_page (n / per_page) m.paginator;
      cursor = n mod per_page;
    }

let reset_selected m = select 0 m

let insert_item n item m =
  let n = max 0 n in
  let rec insert i = function
    | [] -> [ item ]
    | xs when i >= Stdlib.List.length xs -> xs @ [ item ]
    | x :: xs when i = 0 -> item :: x :: xs
    | x :: xs -> x :: insert (i - 1) xs
  in
  set_items (insert n m.items) m

let remove_item n m =
  if n < 0 || n >= Stdlib.List.length m.items then m
  else
    let rec remove i = function
      | [] -> []
      | _ :: xs when i = 0 -> xs
      | x :: xs -> x :: remove (i - 1) xs
    in
    let m = set_items (remove n m.items) m in
    select (index m) m

let set_item n item m =
  if n < 0 || n >= Stdlib.List.length m.items then m
  else
    let updated = Stdlib.List.mapi (fun i x -> if i = n then item else x) m.items in
    set_items updated m

let cursor_up m =
  let current = index m in
  let count = Stdlib.List.length (visible_items m) in
  if count = 0 then m
  else if current > 0 then select (current - 1) m
  else if m.infinite_scrolling then select (count - 1) m
  else m

let cursor_down m =
  let current = index m in
  let count = Stdlib.List.length (visible_items m) in
  if count = 0 then m
  else if current < count - 1 then select (current + 1) m
  else if m.infinite_scrolling then select 0 m
  else m

let next_page m =
  let p = Paginator.next_page m.paginator in
  update_pagination
    {
      m with
      paginator = p;
      cursor =
        clamp m.cursor 0
          (max 0
             (Paginator.items_on_page ~total:(Stdlib.List.length (visible_items m)) p - 1));
    }

let prev_page m =
  let p = Paginator.prev_page m.paginator in
  update_pagination
    {
      m with
      paginator = p;
      cursor =
        clamp m.cursor 0
          (max 0
             (Paginator.items_on_page ~total:(Stdlib.List.length (visible_items m)) p - 1));
    }

let go_to_start m = select 0 m

let go_to_end m =
  let count = Stdlib.List.length (visible_items m) in
  if count = 0 then m else select (count - 1) m

let filter_state m = m.filter_state
let filter_value m = Textinput.value m.filter_input
let setting_filter m = m.filter_state = Filtering
let is_filtered m = m.filter_state = Filter_applied
let filtering_enabled m = m.filtering_enabled

let set_filter_state state m =
  let m =
    {
      m with
      filter_state = state;
      cursor = 0;
      paginator = Paginator.set_page 0 m.paginator;
    }
  in
  let m =
    match state with
    | Unfiltered ->
        { m with filtered_items = []; filter_input = Textinput.blur m.filter_input }
    | Filtering ->
        let input, _ = Textinput.focus m.filter_input in
        recompute_filtered { m with filter_input = input }
    | Filter_applied ->
        let input = Textinput.blur m.filter_input in
        recompute_filtered { m with filter_input = input }
  in
  update_pagination m

let set_filter_text text m =
  let input = Textinput.set_value text m.filter_input in
  let m = { m with filter_input = input; filter_state = Filter_applied; cursor = 0 } in
  update_pagination (recompute_filtered m)

let reset_filter m =
  let input = Textinput.reset m.filter_input |> Textinput.blur in
  update_pagination
    {
      m with
      filter_input = input;
      filter_state = Unfiltered;
      filtered_items = [];
      cursor = 0;
      paginator = Paginator.set_page 0 m.paginator;
    }

let set_filtering_enabled enabled m =
  if enabled then { m with filtering_enabled = true } |> update_pagination
  else reset_filter { m with filtering_enabled = false }

let width m = m.width
let height m = m.height

let set_size ~width ~height m =
  update_pagination { m with width = max 0 width; height = max 0 height }

let set_width width m = set_size ~width ~height:m.height m
let set_height height m = set_size ~width:m.width ~height m
let title m = m.title
let set_title title m = update_pagination { m with title }
let show_title m = m.show_title
let set_show_title value m = update_pagination { m with show_title = value }
let show_filter m = m.show_filter
let set_show_filter value m = update_pagination { m with show_filter = value }
let show_status_bar m = m.show_status_bar
let set_show_status_bar value m = update_pagination { m with show_status_bar = value }
let show_pagination m = m.show_pagination
let set_show_pagination value m = update_pagination { m with show_pagination = value }
let show_help m = m.show_help
let set_show_help value m = update_pagination { m with show_help = value }

let set_status_bar_item_name singular plural m =
  update_pagination { m with item_name_singular = singular; item_name_plural = plural }

let new_status_message message m =
  let generation = m.status_generation + 1 in
  let m = { m with status_message = message; status_generation = generation } in
  let cmd = Cmd.after m.status_message_lifetime (fun () -> Status_timeout generation) in
  (m, cmd)

let start_spinner m = { m with show_spinner = true }
let stop_spinner m = { m with show_spinner = false }
let toggle_spinner m = { m with show_spinner = not m.show_spinner }
let set_spinner kind m = { m with spinner = Spinner.set_kind kind m.spinner }
let set_delegate delegate m = update_pagination { m with delegate }

let disable_quit_keybindings m =
  {
    m with
    keymap =
      {
        m.keymap with
        quit = Key_binding.set_enabled false m.keymap.quit;
        force_quit = Key_binding.set_enabled false m.keymap.force_quit;
      };
    disable_quit = true;
  }

let set_additional_short_help_keys keys m = { m with additional_short_help = keys }
let set_additional_full_help_keys keys m = { m with additional_full_help = keys }
let short_help m = short_help_impl m
let full_help m = full_help_impl m
let paginator m = m.paginator
let keymap m = m.keymap
let styles m = m.styles

let set_styles styles m =
  update_pagination { m with styles; filter_input = set_input_styles { m with styles } }

let infinite_scrolling m = m.infinite_scrolling
let set_infinite_scrolling value m = { m with infinite_scrolling = value }

let update message m =
  let m, cmd =
    match message with
    | Set_items items -> (set_items items m, Cmd.none)
    | Cursor_up -> (cursor_up m, Cmd.none)
    | Cursor_down -> (cursor_down m, Cmd.none)
    | Next_page -> (next_page m, Cmd.none)
    | Prev_page -> (prev_page m, Cmd.none)
    | Go_to_start -> (go_to_start m, Cmd.none)
    | Go_to_end -> (go_to_end m, Cmd.none)
    | Start_filter ->
        if (not m.filtering_enabled) || Stdlib.List.length m.items = 0 then (m, Cmd.none)
        else
          let input, focus_cmd = Textinput.focus m.filter_input in
          let source = { m with filter_state = Filtering; filter_input = input } in
          let filtered_items = (recompute_filtered source).filtered_items in
          let m = { source with filtered_items; cursor = 0 } in
          (update_pagination m, Cmd.map (fun im -> Filter_input im) focus_cmd)
    | Clear_filter -> (reset_filter m, Cmd.none)
    | Cancel_filter -> (reset_filter m, Cmd.none)
    | Accept_filter ->
        if m.filter_state <> Filtering then (m, Cmd.none)
        else if
          Stdlib.List.length m.filtered_items = 0 || Textinput.value m.filter_input = ""
        then (reset_filter m, Cmd.none)
        else (set_filter_state Filter_applied m, Cmd.none)
    | Toggle_full_help ->
        let help = Help.set_show_all (not (Help.show_all m.help)) m.help in
        (update_pagination { m with help }, Cmd.none)
    | Status_timeout generation ->
        if generation = m.status_generation then ({ m with status_message = "" }, Cmd.none)
        else (m, Cmd.none)
    | Spinner spinner_msg ->
        let spinner, spinner_cmd = Spinner.update spinner_msg m.spinner in
        let cmd =
          if m.show_spinner then Cmd.map (fun x -> Spinner x) spinner_cmd else Cmd.none
        in
        ({ m with spinner }, cmd)
    | Filter_input input_msg ->
        if m.filter_state <> Filtering then (m, Cmd.none)
        else
          let input, input_cmd = Textinput.update input_msg m.filter_input in
          let next = { m with filter_input = input } in
          let next =
            if Textinput.value input <> Textinput.value m.filter_input then
              recompute_filtered next
            else next
          in
          (update_pagination next, Cmd.map (fun x -> Filter_input x) input_cmd)
  in
  (m, cmd)

let key m key =
  if m.filter_state = Filtering then
    if Key_binding.matches key m.keymap.cancel_while_filtering then Some Cancel_filter
    else if Key_binding.matches key m.keymap.accept_while_filtering then
      Some Accept_filter
    else Option.map (fun msg -> Filter_input msg) (Textinput.key m.filter_input key)
  else if Key_binding.matches key m.keymap.clear_filter then Some Clear_filter
  else if Key_binding.matches key m.keymap.cursor_up then Some Cursor_up
  else if Key_binding.matches key m.keymap.cursor_down then Some Cursor_down
  else if Key_binding.matches key m.keymap.prev_page then Some Prev_page
  else if Key_binding.matches key m.keymap.next_page then Some Next_page
  else if Key_binding.matches key m.keymap.go_to_start then Some Go_to_start
  else if Key_binding.matches key m.keymap.go_to_end then Some Go_to_end
  else if Key_binding.matches key m.keymap.filter then Some Start_filter
  else if Key_binding.matches key m.keymap.show_full_help then Some Toggle_full_help
  else if Key_binding.matches key m.keymap.close_full_help then Some Toggle_full_help
  else None

let subscriptions m =
  let subs = ref [] in
  if m.show_spinner then
    subs := Sub.map (fun x -> Spinner x) (Spinner.subscriptions m.spinner) :: !subs;
  if m.filter_state = Filtering then
    subs :=
      Sub.map (fun x -> Filter_input x) (Textinput.subscriptions m.filter_input) :: !subs;
  Sub.batch (Stdlib.List.rev !subs)

let view m =
  let sections = ref [] in
  if m.show_title || (m.show_filter && m.filtering_enabled) then
    sections := title_view m :: !sections;
  if m.show_status_bar then sections := status_view m :: !sections;
  let pagination = if m.show_pagination then pagination_view m else "" in
  let help = if m.show_help then help_view m else "" in
  let avail = max 0 (m.height - chrome_height m) in
  let content =
    let visible = visible_items m in
    if visible = [] then
      if m.filter_state = Filtering then ""
      else Style.render m.styles.no_items ("No " ^ m.item_name_plural ^ ".")
    else
      let start, stop =
        Paginator.slice_bounds ~length:(Stdlib.List.length visible) m.paginator
      in
      let page =
        if stop <= start then []
        else Stdlib.List.filteri (fun i _ -> i >= start && i < stop) visible
      in
      let lines =
        Stdlib.List.mapi
          (fun i item ->
            let global = start + i in
            let matched =
              match Stdlib.List.nth_opt m.filtered_items global with
              | Some x -> x.matched
              | None -> []
            in
            let ctx =
              {
                index = global;
                selected = global = index m;
                filter_state = m.filter_state;
                filter_text = filter_value m;
                matched;
                width = m.width;
              }
            in
            m.delegate.render ctx item)
          page
      in
      let body = String.concat (String.make (m.delegate.spacing + 1) '\n') lines in
      let on_page =
        Paginator.items_on_page ~total:(Stdlib.List.length visible) m.paginator
      in
      let pad =
        max 0
          ((on_page - Stdlib.List.length page) * (m.delegate.height + m.delegate.spacing))
      in
      body ^ String.make pad '\n'
  in
  sections := Style.render (Style.height avail Style.empty) content :: !sections;
  if m.show_pagination && pagination <> "" then sections := pagination :: !sections;
  if m.show_help && help <> "" then sections := help :: !sections;
  Layout.join_vertical (Stdlib.List.rev !sections)
