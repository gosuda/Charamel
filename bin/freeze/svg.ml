type rendered = { svg : string; width : float; height : float }

type run = {
  text : string;
  fg : Charamel_ansi.Color.t;
  bg : Charamel_ansi.Color.t;
  bold : bool;
  faint : bool;
  italic : bool;
  underline : Charamel_ansi.Style.underline;
  strike : bool;
  conceal : bool;
  blink : bool;
  reverse : bool;
  link : string option;
}

type style = {
  fg : Charamel_ansi.Color.t;
  bg : Charamel_ansi.Color.t;
  bold : bool;
  faint : bool;
  italic : bool;
  underline : Charamel_ansi.Style.underline;
  strike : bool;
  conceal : bool;
  blink : bool;
  reverse : bool;
  link : string option;
}

let default_style =
  {
    fg = Charamel_ansi.Color.Default;
    bg = Charamel_ansi.Color.Default;
    bold = false;
    faint = false;
    italic = false;
    underline = Charamel_ansi.Style.No_underline;
    strike = false;
    conceal = false;
    blink = false;
    reverse = false;
    link = None;
  }

let style_equal a b =
  Charamel_ansi.Color.equal a.fg b.fg
  && Charamel_ansi.Color.equal a.bg b.bg
  && Bool.equal a.bold b.bold && Bool.equal a.faint b.faint
  && Bool.equal a.italic b.italic && a.underline = b.underline
  && Bool.equal a.strike b.strike
  && Bool.equal a.conceal b.conceal
  && Bool.equal a.blink b.blink
  && Bool.equal a.reverse b.reverse
  && a.link = b.link

let run_of_style style text =
  {
    text;
    fg = style.fg;
    bg = style.bg;
    bold = style.bold;
    faint = style.faint;
    italic = style.italic;
    underline = style.underline;
    strike = style.strike;
    conceal = style.conceal;
    blink = style.blink;
    reverse = style.reverse;
    link = style.link;
  }

let parse_runs text =
  let parser = Charamel_ansi.Parser.create () in
  let actions =
    Charamel_ansi.Parser.feed parser text @ Charamel_ansi.Parser.flush parser
  in
  let lines = ref [] in
  let current = ref [] in
  let style = ref default_style in
  let push_run text =
    if text <> "" then
      match !current with
      | (previous : run) :: rest
        when style_equal !style
               {
                 fg = previous.fg;
                 bg = previous.bg;
                 bold = previous.bold;
                 faint = previous.faint;
                 italic = previous.italic;
                 underline = previous.underline;
                 strike = previous.strike;
                 conceal = previous.conceal;
                 blink = previous.blink;
                 reverse = previous.reverse;
                 link = previous.link;
               } ->
          current := { previous with text = previous.text ^ text } :: rest
      | _ -> current := run_of_style !style text :: !current
  in
  let finish_line () =
    lines := List.rev !current :: !lines;
    current := []
  in
  let clamp value = max 0 (min 255 value) in
  let rgb r g b =
    match Charamel_ansi.Color.rgb (clamp r) (clamp g) (clamp b) with
    | Some value -> value
    | None -> Charamel_ansi.Color.Default
  in
  let param values index =
    match List.nth_opt values index with
    | Some (Some value :: _) -> Some value
    | Some (None :: _) | Some [] | None -> None
  in
  let colon values index subindex =
    Option.bind (List.nth_opt values index) (fun value ->
        Option.bind (List.nth_opt value subindex) Fun.id)
  in
  let update_sgr values =
    let values = if values = [] then [ [ Some 0 ] ] else values in
    let next = ref !style in
    let set_fg value = next := { !next with fg = value } in
    let set_bg value = next := { !next with bg = value } in
    let set_underline value = next := { !next with underline = value } in
    let rec loop index =
      if index >= List.length values then style := !next
      else
        match param values index with
        | None -> loop (index + 1)
        | Some value ->
            let next_index = ref (index + 1) in
            (match value with
            | 0 -> next := default_style
            | 1 -> next := { !next with bold = true }
            | 2 -> next := { !next with faint = true }
            | 3 -> next := { !next with italic = true }
            | 4 ->
                let underline =
                  match colon values index 1 with
                  | Some 2 -> Charamel_ansi.Style.Double
                  | Some 3 -> Charamel_ansi.Style.Curly
                  | Some 4 -> Charamel_ansi.Style.Dotted
                  | Some 5 -> Charamel_ansi.Style.Dashed
                  | _ -> Charamel_ansi.Style.Single
                in
                set_underline underline
            | 5 | 6 -> next := { !next with blink = true }
            | 7 -> next := { !next with reverse = true }
            | 8 -> next := { !next with conceal = true }
            | 9 -> next := { !next with strike = true }
            | 22 -> next := { !next with bold = false; faint = false }
            | 23 -> next := { !next with italic = false }
            | 24 -> set_underline Charamel_ansi.Style.No_underline
            | 25 -> next := { !next with blink = false }
            | 27 -> next := { !next with reverse = false }
            | 28 -> next := { !next with conceal = false }
            | 29 -> next := { !next with strike = false }
            | value when value >= 30 && value <= 37 ->
                set_fg (Charamel_ansi.Color.Basic (value - 30))
            | 39 -> set_fg Charamel_ansi.Color.Default
            | value when value >= 40 && value <= 47 ->
                set_bg (Charamel_ansi.Color.Basic (value - 40))
            | 49 -> set_bg Charamel_ansi.Color.Default
            | value when value >= 90 && value <= 97 ->
                set_fg (Charamel_ansi.Color.Basic (value - 90 + 8))
            | value when value >= 100 && value <= 107 ->
                set_bg (Charamel_ansi.Color.Basic (value - 100 + 8))
            | 38 | 48 | 58 -> (
                let target =
                  match value with
                  | 38 -> `Foreground
                  | 48 -> `Background
                  | _ -> `Underline
                in
                let mode = param values (index + 1) in
                match mode with
                | Some 5 -> (
                    next_index := index + 3;
                    match param values (index + 2) with
                    | Some palette -> (
                        let color = Charamel_ansi.Color.Indexed (clamp palette) in
                        match target with
                        | `Foreground -> set_fg color
                        | `Background -> set_bg color
                        | `Underline -> ())
                    | None -> ())
                | Some 2 -> (
                    next_index := index + 5;
                    let r = Option.value (param values (index + 2)) ~default:0 in
                    let g = Option.value (param values (index + 3)) ~default:0 in
                    let b = Option.value (param values (index + 4)) ~default:0 in
                    let color = rgb r g b in
                    match target with
                    | `Foreground -> set_fg color
                    | `Background -> set_bg color
                    | `Underline -> ())
                | _ -> ())
            | 59 -> ()
            | _ -> ());
            loop !next_index
    in
    loop 0
  in
  let handle = function
    | Charamel_ansi.Parser.Print text -> push_run text
    | Charamel_ansi.Parser.Execute '\n' -> finish_line ()
    | Charamel_ansi.Parser.Execute '\r' -> ()
    | Charamel_ansi.Parser.Execute '\t' -> push_run "\t"
    | Charamel_ansi.Parser.Csi { params; final = 'm'; _ } -> update_sgr params
    | Charamel_ansi.Parser.Osc fields -> (
        match fields with
        | "8" :: _params :: url :: _ ->
            style := { !style with link = (if url = "" then None else Some url) }
        | _ -> ())
    | Charamel_ansi.Parser.Csi _ | Charamel_ansi.Parser.Execute _
    | Charamel_ansi.Parser.Esc _ | Charamel_ansi.Parser.Dcs _ | Charamel_ansi.Parser.Apc _
    | Charamel_ansi.Parser.Pm _ | Charamel_ansi.Parser.Sos _ ->
        ()
  in
  List.iter handle actions;
  finish_line ();
  List.rev !lines

let theme_for name =
  match String.lowercase_ascii name with
  | "dracula" -> Charamel_highlight.Theme.dracula
  | "github" -> Charamel_highlight.Theme.github ~is_dark:false
  | "github-dark" -> Charamel_highlight.Theme.github ~is_dark:true
  | "monokai" -> Charamel_highlight.Theme.monokai
  | "nord" -> Charamel_highlight.Theme.nord
  | "solarized-light" -> Charamel_highlight.Theme.solarized ~is_dark:false
  | "solarized-dark" -> Charamel_highlight.Theme.solarized ~is_dark:true
  | _ -> Charamel_highlight.Theme.charm ~is_dark:true

let xml_escape text =
  let buffer = Buffer.create (String.length text + 8) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | '\'' -> Buffer.add_string buffer "&apos;"
      | character -> Buffer.add_char buffer character)
    text;
  Buffer.contents buffer

let color_hex color =
  Option.map
    (fun (r, g, b) -> Fmt.str "#%02X%02X%02X" r g b)
    (Charamel_ansi.Color.to_rgb color)

let attr name value = Fmt.str " %s=\"%s\"" name (xml_escape value)
let float value = Fmt.str "%.2f" value

let visual_width ~tab_width text =
  let base =
    Charamel_ansi.Text.width (String.map (fun c -> if c = '\t' then ' ' else c) text)
  in
  let tabs =
    String.fold_left
      (fun count character -> if character = '\t' then count + 1 else count)
      0 text
  in
  base + (tabs * (max 1 tab_width - 1))

let style_attrs (run : run) =
  let buffer = Buffer.create 64 in
  Buffer.add_string buffer (attr "xml:space" "preserve");
  Option.iter
    (fun color -> Buffer.add_string buffer (attr "fill" color))
    (color_hex run.fg);
  if run.bold then Buffer.add_string buffer (attr "font-weight" "bold");
  (match if run.conceal then Some "0" else if run.faint then Some "0.65" else None with
  | Some opacity -> Buffer.add_string buffer (attr "opacity" opacity)
  | None -> ());
  if run.italic then Buffer.add_string buffer (attr "font-style" "italic");
  let decorations =
    (if run.underline <> Charamel_ansi.Style.No_underline then [ "underline" ] else [])
    @ if run.strike then [ "line-through" ] else []
  in
  if decorations <> [] then
    Buffer.add_string buffer (attr "text-decoration" (String.concat " " decorations));
  Buffer.contents buffer

let render_runs ~char_width ~line_height ~x ~y ~tab_width runs =
  let text = Buffer.create 128 in
  let backgrounds = Buffer.create 128 in
  let column = ref 0 in
  List.iter
    (fun (run : run) ->
      let width = visual_width ~tab_width run.text in
      (match color_hex run.bg with
      | None -> ()
      | Some color ->
          Buffer.add_string backgrounds
            (Fmt.str "<rect%s%s%s%s%s/>" (attr "fill" color)
               (attr "x" (float (x +. (float_of_int !column *. char_width))))
               (attr "y" (float (y -. line_height +. 1.)))
               (attr "width"
                  (float
                     ((float_of_int width *. char_width)
                     +. if width = 0 then 0. else char_width *. 0.5)))
               (attr "height" (float (line_height +. 1.)))));
      let body = xml_escape run.text in
      let span = Fmt.str "<tspan%s>%s</tspan>" (style_attrs run) body in
      (match run.link with
      | None -> Buffer.add_string text span
      | Some url -> Buffer.add_string text (Fmt.str "<a%s>%s</a>" (attr "href" url) span));
      column := !column + width)
    runs;
  (Buffer.contents backgrounds, Buffer.contents text)

let read_font_bytes path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let length = in_channel_length ic in
      let bytes = Bytes.create length in
      really_input ic bytes 0 length;
      Bytes.unsafe_to_string bytes)

let read_font ~fs_root config =
  if config.Config.font.Config.file = "" then Ok ""
  else
    let path =
      if Filename.is_relative config.Config.font.Config.file then
        Filename.concat fs_root config.Config.font.Config.file
      else config.Config.font.Config.file
    in
    try
      let bytes = read_font_bytes path in
      let encoded = Base64.encode_string bytes in
      let extension =
        String.lowercase_ascii (Filename.extension config.Config.font.Config.file)
      in
      let format =
        match extension with
        | ".ttf" -> "truetype"
        | ".woff" -> "woff"
        | ".woff2" -> "woff2"
        | _ -> ""
      in
      if format = "" then Error (Fmt.str "%s is not a supported font extension" extension)
      else
        Ok
          (Fmt.str
             "<style>@font-face{font-family:'%s';src:url(data:font/%s;base64,%s) \
              format('%s');}</style>"
             (xml_escape config.Config.font.Config.family)
             format encoded format)
    with
    | Sys_error _ ->
        Error (Fmt.str "could not read font %s" config.Config.font.Config.file)
    | Unix.Unix_error (error, function_name, argument) ->
        Error
          (Fmt.str "could not read font %s: %s (%s %s)" config.Config.font.Config.file
             (Unix.error_message error) function_name argument)

let render ~fs_root ~(config : Config.t) ~language ~text ~is_ansi =
  let source =
    if is_ansi then text
    else
      match language with
      | None -> text
      | Some spec ->
          Charamel_highlight.render ~theme:(theme_for config.Config.theme) spec text
  in
  let source =
    if config.Config.wrap > 0 then
      Charamel_ansi.Text.wrap ~width:config.Config.wrap source
    else source
  in
  let line_selection = List.map (fun line -> line - 1) config.Config.lines in
  let source = Input.cut_lines ~lines:line_selection source in
  let source_plain = Charamel_ansi.Text.strip source in
  if source_plain = "" then Error "No input"
  else
    let lines = parse_runs source in
    let line_count = max 1 (List.length lines) in
    let tab_width = if is_ansi then 6 else 4 in
    let longest =
      lines
      |> List.fold_left
           (fun longest line ->
             let value =
               List.fold_left
                 (fun width run -> width + visual_width ~tab_width run.text)
                 0 line
             in
             max longest value)
           0
    in
    let output_is_png = Filename.check_suffix config.Config.output ".png" in
    let scale =
      if config.Config.width = 0. && config.Config.height = 0. && output_is_png then 4.
      else 1.
    in
    let margin = Config.expand_sides ~scale config.Config.margin in
    let padding = Config.expand_sides ~scale config.Config.padding in
    let padding = Array.copy padding in
    if config.Config.window then padding.(0) <- padding.(0) +. (15. *. scale);
    let char_width = config.Config.font.Config.size /. 1.68 *. scale in
    let line_height =
      max 0.1 (config.Config.font.Config.size *. config.Config.line_height *. scale)
    in
    let text_width = float_of_int (longest + 1) *. char_width in
    let text_height = float_of_int line_count *. line_height in
    let horizontal_margin = margin.(1) +. margin.(3) in
    let vertical_margin = margin.(0) +. margin.(2) in
    let horizontal_padding = padding.(1) +. padding.(3) in
    let vertical_padding = padding.(0) +. padding.(2) in
    let auto_width = config.Config.width = 0. in
    let auto_height = config.Config.height = 0. in
    let image_width =
      if auto_width then text_width +. horizontal_padding +. horizontal_margin
      else config.Config.width
    in
    let image_height =
      if auto_height then text_height +. vertical_padding +. vertical_margin
      else config.Config.height
    in
    let terminal_width =
      if auto_width then text_width +. horizontal_padding
      else config.Config.width -. horizontal_margin
    in
    let terminal_height =
      if auto_height then text_height +. vertical_padding
      else config.Config.height -. vertical_margin
    in
    let terminal_width = max 1. terminal_width in
    let terminal_height = max 1. terminal_height in
    let line_number_width =
      if config.Config.show_line_numbers then
        config.Config.font.Config.size *. 3. *. scale
      else 0.
    in
    let terminal_width =
      if config.Config.show_line_numbers then terminal_width +. line_number_width
      else terminal_width
    in
    let image_width =
      if config.Config.show_line_numbers && auto_width then
        image_width +. line_number_width
      else image_width
    in
    let border_width = max 0. config.Config.border.Config.width in
    let terminal_width =
      if border_width > 0. then terminal_width -. (2. *. border_width) else terminal_width
    in
    let terminal_height =
      if border_width > 0. then terminal_height -. (2. *. border_width)
      else terminal_height
    in
    let terminal_width = max 1. terminal_width
    and terminal_height = max 1. terminal_height in
    let terminal_x = max margin.(3) (border_width /. 2.) in
    let terminal_y = max margin.(0) (border_width /. 2.) in
    let font_css = read_font ~fs_root config in
    match font_css with
    | Error _ as error -> error
    | Ok font_css ->
        let buffer = Buffer.create (String.length source + 512) in
        Buffer.add_string buffer
          (Fmt.str
             "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%s\" height=\"%s\" \
              viewBox=\"0 0 %s %s\">"
             (float image_width) (float image_height) (float image_width)
             (float image_height));
        Buffer.add_string buffer "<defs>";
        Buffer.add_string buffer font_css;
        Buffer.add_string buffer
          (Fmt.str "<clipPath id=\"terminalMask\"><rect%s%s%s%s/></clipPath>"
             (attr "x" (float margin.(3)))
             (attr "y" (float margin.(0)))
             (attr "width" (float terminal_width))
             (attr "height" (float terminal_height)));
        if
          config.Config.shadow.Config.blur > 0.
          || config.Config.shadow.Config.x <> 0.
          || config.Config.shadow.Config.y <> 0.
        then
          Buffer.add_string buffer
            (Fmt.str
               "<filter id=\"shadow\" \
                filterUnits=\"userSpaceOnUse\"><feGaussianBlur%s%s/><feOffset%s%s%s/><feMerge><feMergeNode/><feMergeNode%s/></feMerge></filter>"
               (attr "in" "SourceAlpha")
               (attr "stdDeviation" (float (config.Config.shadow.Config.blur *. scale)))
               (attr "result" "offsetblur")
               (attr "dx" (float (config.Config.shadow.Config.x *. scale)))
               (attr "dy" (float (config.Config.shadow.Config.y *. scale)))
               (attr "in" "SourceGraphic"));
        Buffer.add_string buffer "</defs>";
        let terminal_attrs =
          attr "x" (float terminal_x)
          ^ attr "y" (float terminal_y)
          ^ attr "width" (float terminal_width)
          ^ attr "height" (float terminal_height)
          ^ attr "fill"
              ( Option.value
                  (Charamel_ansi.Color.of_hex config.Config.background)
                  ~default:Charamel_ansi.Color.Default
              |> fun color -> Option.value (color_hex color) ~default:"#171717" )
        in
        let radius =
          if config.Config.border.Config.radius > 0. then
            attr "rx" (float (config.Config.border.Config.radius *. scale))
            ^ attr "ry" (float (config.Config.border.Config.radius *. scale))
          else ""
        in
        let outline =
          if border_width > 0. then
            attr "stroke" config.Config.border.Config.color
            ^ attr "stroke-width" (float border_width)
          else ""
        in
        let filter =
          if
            config.Config.shadow.Config.blur > 0.
            || config.Config.shadow.Config.x <> 0.
            || config.Config.shadow.Config.y <> 0.
          then attr "filter" "url(#shadow)"
          else ""
        in
        Buffer.add_string buffer
          (Fmt.str "<rect%s%s%s%s/>" terminal_attrs radius outline filter);
        if config.Config.window then begin
          Buffer.add_string buffer
            (Fmt.str "<svg%s%s><circle%s%s%s/><circle%s%s%s/><circle%s%s%s/></svg>"
               (attr "x" (float margin.(3)))
               (attr "y" (float margin.(0)))
               (attr "cx" (float ((19. *. scale) -. (5.5 *. scale))))
               (attr "cy" (float (12. *. scale)))
               (attr "r" (float (5.5 *. scale)))
               (attr "cx" (float ((38. *. scale) -. (5.5 *. scale))))
               (attr "cy" (float (12. *. scale)))
               (attr "r" (float (5.5 *. scale)))
               (attr "cx" (float ((57. *. scale) -. (5.5 *. scale))))
               (attr "cy" (float (12. *. scale)))
               (attr "r" (float (5.5 *. scale))));
          Buffer.add_string buffer
            (Fmt.str
               "<style>circle:nth-child(1){fill:#FF5A54}circle:nth-child(2){fill:#E6BF29}circle:nth-child(3){fill:#52C12B}</style>")
        end;
        let default_fill = if is_ansi then attr "fill" "#C4C4C4" else "" in
        Buffer.add_string buffer
          (Fmt.str "<g%s%s%s%s>"
             (attr "font-family" config.Config.font.Config.family)
             (attr "font-size" (float (config.Config.font.Config.size *. scale)))
             (attr "font-variant-ligatures"
                (if config.Config.font.Config.ligatures then "normal" else "none"))
             default_fill);
        let offset =
          match config.Config.lines with first :: _ -> max 0 (first - 1) | [] -> 0
        in
        List.iteri
          (fun index line ->
            let y =
              margin.(0) +. padding.(0) +. (float_of_int (index + 1) *. line_height)
            in
            if y <= image_height -. margin.(2) -. padding.(2) then begin
              let background, text =
                render_runs ~char_width ~line_height
                  ~x:(margin.(3) +. padding.(3) +. line_number_width)
                  ~y ~tab_width line
              in
              Buffer.add_string buffer background;
              Buffer.add_string buffer
                (Fmt.str "<text%s%s%s>"
                   (attr "x" (float (margin.(3) +. padding.(3) +. line_number_width)))
                   (attr "y" (float y))
                   (attr "clip-path" "url(#terminalMask)"));
              if config.Config.show_line_numbers then
                Buffer.add_string buffer
                  (Fmt.str "<tspan%s>%s</tspan>" (attr "xml:space" "preserve")
                     (xml_escape (Fmt.str "%3d  " (index + 1 + offset))));
              Buffer.add_string buffer text;
              Buffer.add_string buffer "</text>"
            end)
          lines;
        Buffer.add_string buffer "</g></svg>";
        Ok { svg = Buffer.contents buffer; width = image_width; height = image_height }
