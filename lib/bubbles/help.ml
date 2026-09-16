module Style = Charm_lipgloss.Style
module Layout = Charm_lipgloss.Layout
module Text = Charm_ansi.Text

let color_hex value =
  match Charm_ansi.Color.of_hex value with
  | Some color -> color
  | None -> Charm_ansi.Color.Default

type styles = {
  ellipsis : Style.t;
  short_key : Style.t;
  short_desc : Style.t;
  short_separator : Style.t;
  full_key : Style.t;
  full_desc : Style.t;
  full_separator : Style.t;
}

let default_styles ~is_dark =
  let light_dark ~light ~dark =
    Charm_lipgloss.light_dark ~is_dark ~light:(color_hex light) ~dark:(color_hex dark)
  in
  let key = Style.foreground (light_dark ~light:"#909090" ~dark:"#626262") Style.empty in
  let description =
    Style.foreground (light_dark ~light:"#B2B2B2" ~dark:"#4A4A4A") Style.empty
  in
  let separator =
    Style.foreground (light_dark ~light:"#DADADA" ~dark:"#3C3C3C") Style.empty
  in
  {
    ellipsis = separator;
    short_key = key;
    short_desc = description;
    short_separator = separator;
    full_key = key;
    full_desc = description;
    full_separator = separator;
  }

type keymap = { short_help : Key_binding.t list; full_help : Key_binding.t list list }

type t = {
  width : int;
  show_all : bool;
  short_separator : string;
  full_separator : string;
  ellipsis : string;
  styles : styles;
}

let v ?(width = 0) ?(show_all = false) ?(short_separator = " • ")
    ?(full_separator = "    ") ?(ellipsis = "…") ?(is_dark = true) ?styles () =
  let styles =
    match styles with Some styles -> styles | None -> default_styles ~is_dark
  in
  { width = max 0 width; show_all; short_separator; full_separator; ellipsis; styles }

let width t = t.width
let set_width width t = { t with width = max 0 width }
let show_all t = t.show_all
let set_show_all show_all t = { t with show_all }
let set_styles styles t = { t with styles }
let styles t = t.styles
let enabled binding = Key_binding.enabled binding

let should_add_item t ~total_width ~item_width =
  if t.width > 0 && total_width + item_width > t.width then
    let tail = " " ^ Style.render (Style.inline true t.styles.ellipsis) t.ellipsis in
    if total_width + Text.width tail < t.width then (tail, false) else ("", true)
  else ("", true)

let short_view t bindings =
  let separator =
    Style.render (Style.inline true t.styles.short_separator) t.short_separator
  in
  let output = Buffer.create 64 in
  let total_width = ref 0 in
  let rendered_any = ref false in
  let finished = ref false in
  Stdlib.List.iter
    (fun binding ->
      if (not !finished) && enabled binding then begin
        let key, description = (binding : Key_binding.t).Key_binding.help in
        let prefix = if !rendered_any then separator else "" in
        let item =
          prefix
          ^ Style.render (Style.inline true t.styles.short_key) key
          ^ " "
          ^ Style.render (Style.inline true t.styles.short_desc) description
        in
        let item_width = Text.width item in
        let tail, add_item = should_add_item t ~total_width:!total_width ~item_width in
        if add_item then begin
          Buffer.add_string output item;
          total_width := !total_width + item_width;
          rendered_any := true
        end
        else begin
          if tail <> "" then Buffer.add_string output tail;
          finished := true
        end
      end)
    bindings;
  Buffer.contents output

let full_view t groups =
  let separator =
    Style.render (Style.inline true t.styles.full_separator) t.full_separator
  in
  let columns = ref [] in
  let total_width = ref 0 in
  let finished = ref false in
  let rendered_any = ref false in
  Stdlib.List.iter
    (fun group ->
      if not !finished then begin
        let enabled_bindings = Stdlib.List.filter enabled group in
        if enabled_bindings <> [] then begin
          let keys, descriptions =
            Stdlib.List.split
              (Stdlib.List.map
                 (fun binding -> (binding : Key_binding.t).Key_binding.help)
                 enabled_bindings)
          in
          let prefix = if !rendered_any then separator else "" in
          let column =
            Layout.join_horizontal
              [
                prefix;
                Style.render t.styles.full_key (String.concat "\n" keys);
                " ";
                Style.render t.styles.full_desc (String.concat "\n" descriptions);
              ]
          in
          let column_width = Layout.width column in
          let tail, add_item =
            should_add_item t ~total_width:!total_width ~item_width:column_width
          in
          if add_item then begin
            columns := column :: !columns;
            total_width := !total_width + column_width;
            rendered_any := true
          end
          else begin
            if tail <> "" then columns := tail :: !columns;
            finished := true
          end
        end
      end)
    groups;
  Layout.join_horizontal (Stdlib.List.rev !columns)

let view t keymap =
  if t.show_all then full_view t keymap.full_help else short_view t keymap.short_help
