(* Theme literals follow Charmbracelet glamour v2.0.1 MIT styles/{ascii,dark,light,
   pink,dracula,tokyo-night}.json and the corresponding styles/*.go values.
   The full GitHub shortcode table is in emoji.ml; it is generated from
   yuin/goldmark-emoji v1.0.5, pinned by glamour's go.sum. *)
module Color = Charm_ansi.Color

module Theme = struct
  type block = {
    prefix : string;
    suffix : string;
    indent : int;
    margin : int;
    color : Color.t option;
    background : Color.t option;
    bold : bool;
    italic : bool;
    underline : bool;
    faint : bool;
    strike : bool;
    block_prefix : string;
    block_suffix : string;
  }

  type t = {
    document : block;
    block_quote : block;
    paragraph : block;
    list : block * int;
    heading : block;
    h1 : block;
    h2 : block;
    h3 : block;
    h4 : block;
    h5 : block;
    h6 : block;
    text : block;
    strong : block;
    emph : block;
    strike : block;
    code : block;
    code_block : block * Charm_highlight.Theme.t;
    hr : block;
    link : block;
    link_text : block;
    image : block;
    image_text : block;
    table : block * string;
    task_ticked : string;
    task_unticked : string;
    html_block : block;
    html_span : block;
    definition_term : block;
    definition_description : block;
    item : string;
    enumeration : string;
  }

  let block ?(prefix = "") ?(suffix = "") ?(indent = 0) ?(margin = 0) ?(color = None)
      ?(background = None) ?(bold = false) ?(italic = false) ?(underline = false)
      ?(faint = false) ?(strike = false) ?(block_prefix = "") ?(block_suffix = "") () =
    {
      prefix;
      suffix;
      indent;
      margin;
      color;
      background;
      bold;
      italic;
      underline;
      faint;
      strike;
      block_prefix;
      block_suffix;
    }

  let empty = block ()
  let indexed n = Some (Color.Indexed n)
  let rgb r g b = Some (Color.Rgb (r, g, b))
  let code_no_highlight _ = Charm_lipgloss.Style.empty
  let code_charm_dark = Charm_highlight.Theme.charm ~is_dark:true
  let code_charm_light = Charm_highlight.Theme.charm ~is_dark:false
  let code_dracula = Charm_highlight.Theme.dracula

  let tokyo_color r g b =
    Charm_lipgloss.Style.foreground (Color.Rgb (r, g, b)) Charm_lipgloss.Style.empty

  let code_tokyo = function
    | Charm_highlight.Keyword -> tokyo_color 0x2a 0xc3 0xde
    | Charm_highlight.Type -> tokyo_color 0x7a 0xa2 0xf7
    | Charm_highlight.Builtin -> tokyo_color 0x7a 0xa2 0xf7
    | Charm_highlight.Constant -> tokyo_color 0xbb 0x9a 0xf7
    | Charm_highlight.String -> tokyo_color 0xe0 0xaf 0x68
    | Charm_highlight.Number -> tokyo_color 0xbb 0x9a 0xf7
    | Charm_highlight.Comment -> tokyo_color 0x56 0x5f 0x89
    | Charm_highlight.Operator -> tokyo_color 0x2a 0xc3 0xde
    | Charm_highlight.Punct -> tokyo_color 0xa9 0xb1 0xd6
    | Charm_highlight.Ident -> tokyo_color 0xa9 0xb1 0xd6
    | Charm_highlight.Attribute -> tokyo_color 0x9e 0xce 0x6a
    | Charm_highlight.Text -> tokyo_color 0xa9 0xb1 0xd6

  let dark =
    let document =
      block ~block_prefix:"\n" ~block_suffix:"\n" ~margin:2 ~color:(indexed 252) ()
    in
    let block_quote = block ~indent:1 ~block_prefix:"│ " () in
    let paragraph = empty in
    let list = (empty, 2) in
    let heading = block ~block_suffix:"\n" ~color:(indexed 39) ~bold:true () in
    let h1 =
      block ~prefix:" " ~suffix:" " ~color:(indexed 228) ~background:(indexed 63)
        ~bold:true ()
    in
    let h2 = block ~prefix:"## " () in
    let h3 = block ~prefix:"### " () in
    let h4 = block ~prefix:"#### " () in
    let h5 = block ~prefix:"##### " () in
    let h6 = block ~prefix:"###### " ~color:(indexed 35) () in
    let text = empty in
    let strong = block ~bold:true () in
    let emph = block ~italic:true () in
    let strike = block ~strike:true () in
    let hr = block ~prefix:"\n--------\n" ~color:(indexed 240) () in
    let code =
      block ~prefix:"\u{00A0}" ~suffix:"\u{00A0}" ~color:(indexed 203)
        ~background:(indexed 236) ()
    in
    let code_block = block ~margin:2 ~color:(indexed 244) () in
    let link = block ~color:(indexed 30) ~underline:true () in
    let link_text = block ~color:(indexed 35) ~bold:true () in
    let image = block ~color:(indexed 212) ~underline:true () in
    let image_text = block ~color:(indexed 243) ~prefix:"Image: " ~suffix:" →" () in
    let table = (empty, "") in
    let task_ticked = "[✓] " and task_unticked = "[ ] " in
    let definition_description = block ~block_prefix:"\n🠶 " () in
    {
      document;
      block_quote;
      paragraph;
      list;
      heading;
      h1;
      h2;
      h3;
      h4;
      h5;
      h6;
      text;
      strong;
      emph;
      strike;
      code;
      code_block = (code_block, code_charm_dark);
      hr;
      link;
      link_text;
      image;
      image_text;
      table;
      task_ticked;
      task_unticked;
      html_block = empty;
      html_span = empty;
      definition_term = empty;
      definition_description;
      item = "• ";
      enumeration = ". ";
    }

  let light =
    let document =
      block ~block_prefix:"\n" ~block_suffix:"\n" ~margin:2 ~color:(indexed 234) ()
    in
    let block_quote = block ~indent:1 ~block_prefix:"│ " () in
    let list = (empty, 2) in
    let heading = block ~block_suffix:"\n" ~color:(indexed 27) ~bold:true () in
    let h1 =
      block ~prefix:" " ~suffix:" " ~color:(indexed 228) ~background:(indexed 63)
        ~bold:true ()
    in
    let h2 = block ~prefix:"## " () in
    let h3 = block ~prefix:"### " () in
    let h4 = block ~prefix:"#### " () in
    let h5 = block ~prefix:"##### " () in
    let h6 = block ~prefix:"###### " () in
    let strong = block ~bold:true () in
    let emph = block ~italic:true () in
    let strike = block ~strike:true () in
    let hr = block ~prefix:"\n--------\n" ~color:(indexed 249) () in
    let link = block ~color:(indexed 36) ~underline:true () in
    let link_text = block ~color:(indexed 29) ~bold:true () in
    let image = block ~color:(indexed 205) ~underline:true () in
    let image_text = block ~color:(indexed 243) ~prefix:"Image: " ~suffix:" →" () in
    let code =
      block ~prefix:"\u{00A0}" ~suffix:"\u{00A0}" ~color:(indexed 203)
        ~background:(indexed 254) ()
    in
    let code_block = block ~margin:2 ~color:(indexed 242) () in
    let definition_description = block ~block_prefix:"\n🠶 " () in
    {
      document;
      block_quote;
      paragraph = empty;
      list;
      heading;
      h1;
      h2;
      h3;
      h4;
      h5;
      h6;
      text = empty;
      strong;
      emph;
      strike;
      code;
      code_block = (code_block, code_charm_light);
      hr;
      link;
      link_text;
      image;
      image_text;
      table = (empty, "");
      task_ticked = "[✓] ";
      task_unticked = "[ ] ";
      html_block = empty;
      html_span = empty;
      definition_term = empty;
      definition_description;
      item = "• ";
      enumeration = ". ";
    }

  let dracula =
    let document =
      block ~block_prefix:"\n" ~block_suffix:"\n" ~margin:2 ~color:(rgb 0xf8 0xf8 0xf2) ()
    in
    let block_quote = block ~indent:2 ~color:(rgb 0xf1 0xfa 0x8c) ~italic:true () in
    let list = (block ~color:(rgb 0xf8 0xf8 0xf2) (), 2) in
    let heading = block ~block_suffix:"\n" ~color:(rgb 0xbd 0x93 0xf9) ~bold:true () in
    let h1 = block ~prefix:"# " () in
    let h2 = block ~prefix:"## " () in
    let h3 = block ~prefix:"### " () in
    let h4 = block ~prefix:"#### " () in
    let h5 = block ~prefix:"##### " () in
    let h6 = block ~prefix:"###### " () in
    let strong = block ~color:(rgb 0xff 0xb8 0x6c) ~bold:true () in
    let emph = block ~color:(rgb 0xf1 0xfa 0x8c) ~italic:true () in
    let strike = block ~strike:true () in
    let hr = block ~prefix:"\n--------\n" ~color:(rgb 0x62 0x72 0xa4) () in
    let link = block ~color:(rgb 0x8b 0xe9 0xfd) ~underline:true () in
    let link_text = block ~color:(rgb 0xff 0x79 0xc6) () in
    let image = block ~color:(rgb 0x8b 0xe9 0xfd) ~underline:true () in
    let image_text =
      block ~color:(rgb 0xff 0x79 0xc6) ~prefix:"Image: " ~suffix:" →" ()
    in
    let code = block ~color:(rgb 0x50 0xfa 0x7b) () in
    let code_block = block ~margin:2 ~color:(rgb 0xff 0xb8 0x6c) () in
    let definition_description = block ~block_prefix:"\n🠶 " () in
    {
      document;
      block_quote;
      paragraph = empty;
      list;
      heading;
      h1;
      h2;
      h3;
      h4;
      h5;
      h6;
      text = empty;
      strong;
      emph;
      strike;
      code;
      code_block = (code_block, code_dracula);
      hr;
      link;
      link_text;
      image;
      image_text;
      table = (empty, "");
      task_ticked = "[✓] ";
      task_unticked = "[ ] ";
      html_block = empty;
      html_span = empty;
      definition_term = empty;
      definition_description;
      item = "• ";
      enumeration = ". ";
    }

  let tokyo_night =
    let document =
      block ~block_prefix:"\n" ~block_suffix:"\n" ~margin:2 ~color:(rgb 0xa9 0xb1 0xd6) ()
    in
    let block_quote = block ~indent:1 ~block_prefix:"│ " () in
    let list = (block ~color:(rgb 0xa9 0xb1 0xd6) (), 2) in
    let heading = block ~block_suffix:"\n" ~color:(rgb 0xbb 0x9a 0xf7) ~bold:true () in
    let h1 = block ~prefix:"# " ~bold:true () in
    let h2 = block ~prefix:"## " () in
    let h3 = block ~prefix:"### " () in
    let h4 = block ~prefix:"#### " () in
    let h5 = block ~prefix:"##### " () in
    let h6 = block ~prefix:"###### " () in
    let strong = block ~bold:true () in
    let emph = block ~italic:true () in
    let strike = block ~strike:true () in
    let hr = block ~prefix:"\n--------\n" ~color:(rgb 0x56 0x5f 0x89) () in
    let link = block ~color:(rgb 0x7a 0xa2 0xf7) ~underline:true () in
    let link_text = block ~color:(rgb 0x2a 0xc3 0xde) () in
    let image = block ~color:(rgb 0x7a 0xa2 0xf7) ~underline:true () in
    let image_text =
      block ~color:(rgb 0x2a 0xc3 0xde) ~prefix:"Image: " ~suffix:" →" ()
    in
    let code = block ~color:(rgb 0x9e 0xce 0x6a) () in
    let code_block = block ~margin:2 ~color:(rgb 0xff 0x9e 0x64) () in
    let definition_description = block ~block_prefix:"\n🠶 " () in
    {
      document;
      block_quote;
      paragraph = empty;
      list;
      heading;
      h1;
      h2;
      h3;
      h4;
      h5;
      h6;
      text = empty;
      strong;
      emph;
      strike;
      code;
      code_block = (code_block, code_tokyo);
      hr;
      link;
      link_text;
      image;
      image_text;
      table = (empty, "");
      task_ticked = "[✓] ";
      task_unticked = "[ ] ";
      html_block = empty;
      html_span = empty;
      definition_term = empty;
      definition_description;
      item = "• ";
      enumeration = ". ";
    }

  let pink =
    let document = block ~margin:2 () in
    let block_quote = block ~indent:1 ~block_prefix:"│ " () in
    let list = (empty, 2) in
    let heading = block ~block_suffix:"\n" ~color:(indexed 212) ~bold:true () in
    let h1 = block ~block_prefix:"\n" ~block_suffix:"\n" () in
    let h2 = block ~prefix:"▌ " () in
    let h3 = block ~prefix:"┃ " () in
    let h4 = block ~prefix:"│ " () in
    let h5 = block ~prefix:"┆ " () in
    let h6 = block ~prefix:"┊ " () in
    let strong = block ~bold:true () in
    let emph = block ~italic:true () in
    let strike = block ~strike:true () in
    let hr = block ~prefix:"\n──────\n" ~color:(indexed 212) () in
    let link = block ~color:(indexed 99) ~underline:true () in
    let link_text = block ~bold:true () in
    let image = block ~underline:true () in
    let image_text = block ~prefix:"Image: " () in
    let code =
      block ~prefix:"\u{00A0}" ~suffix:"\u{00A0}" ~color:(indexed 212)
        ~background:(indexed 236) ()
    in
    let code_block = empty in
    let definition_description = block ~block_prefix:"\n🠶 " () in
    {
      document;
      block_quote;
      paragraph = empty;
      list;
      heading;
      h1;
      h2;
      h3;
      h4;
      h5;
      h6;
      text = empty;
      strong;
      emph;
      strike;
      code;
      code_block = (code_block, code_no_highlight);
      hr;
      link;
      link_text;
      image;
      image_text;
      table = (empty, "");
      task_ticked = "[✓] ";
      task_unticked = "[ ] ";
      html_block = empty;
      html_span = empty;
      definition_term = empty;
      definition_description;
      item = "• ";
      enumeration = ". ";
    }

  let ascii =
    let document = block ~block_prefix:"\n" ~block_suffix:"\n" ~margin:2 () in
    let block_quote = block ~indent:1 ~block_prefix:"| " () in
    let list = (empty, 4) in
    let heading = block ~block_suffix:"\n" () in
    let h1 = block ~prefix:"# " () in
    let h2 = block ~prefix:"## " () in
    let h3 = block ~prefix:"### " () in
    let h4 = block ~prefix:"#### " () in
    let h5 = block ~prefix:"##### " () in
    let h6 = block ~prefix:"###### " () in
    let strong = block ~block_prefix:"**" ~block_suffix:"**" () in
    let emph = block ~block_prefix:"*" ~block_suffix:"*" () in
    let strike = block ~block_prefix:"~~" ~block_suffix:"~~" () in
    let hr = block ~prefix:"\n--------\n" () in
    let code = block ~block_prefix:"`" ~block_suffix:"`" () in
    let code_block = block ~margin:2 () in
    let definition_description = block ~block_prefix:"\n* " () in
    {
      document;
      block_quote;
      paragraph = empty;
      list;
      heading;
      h1;
      h2;
      h3;
      h4;
      h5;
      h6;
      text = empty;
      strong;
      emph;
      strike;
      code;
      code_block = (code_block, code_no_highlight);
      hr;
      link = empty;
      link_text = empty;
      image = empty;
      image_text = block ~prefix:"Image: " ~suffix:" →" ();
      table = (empty, "|");
      task_ticked = "[x] ";
      task_unticked = "[ ] ";
      html_block = empty;
      html_span = empty;
      definition_term = empty;
      definition_description;
      item = "• ";
      enumeration = ". ";
    }

  let notty = ascii
  let auto ~is_dark = if is_dark then dark else light
end

type error = [ `Markdown of string ]
type table_link = { content : string; href : string; image : bool }

type context = {
  width : int;
  theme : Theme.t;
  base_url : string;
  preserve_newlines : bool;
  emoji : bool;
  table_wrap : bool;
  defs : Cmarkit.Label.defs;
  footnotes : (string * Cmarkit.Block.Footnote.t) list;
  mutable used_footnotes : string list;
  mutable in_table : bool;
  mutable table_links : table_link list;
}

let style_of_block (b : Theme.block) =
  let open Charm_lipgloss.Style in
  let s = empty in
  let s = match b.Theme.color with None -> s | Some c -> foreground c s in
  let s = match b.Theme.background with None -> s | Some c -> background c s in
  let s = bold b.Theme.bold s in
  let s = italic b.Theme.italic s in
  let s = underline b.Theme.underline s in
  let s = faint b.Theme.faint s in
  strikethrough b.Theme.strike s

let style_text (b : Theme.block) text =
  if text = "" then "" else Charm_lipgloss.Style.render (style_of_block b) text

let wrap_text width s =
  if width < 1 then s else Charm_ansi.Text.wrap ~breakpoints:" ,.;-+|" ~width s

let split_lines s = String.split_on_char '\n' s
let spaces n = if n <= 0 then "" else String.make n ' '
let uri_of_string s = try Some (Uri.of_string s) with Invalid_argument _ -> None
let uri_host s = Option.bind (uri_of_string s) Uri.host

let resolve_url ~base_url rel =
  if rel = "" || String.starts_with ~prefix:"#" rel then rel
  else
    match uri_of_string rel with
    | None -> rel
    | Some parsed -> (
        match Uri.scheme parsed with
        | Some _ -> rel
        | None when base_url = "" -> rel
        | None -> (
            match uri_of_string base_url with
            | None -> rel
            | Some base -> Uri.to_string (Uri.resolve "https" base parsed)))

let url_is_valid url =
  let has_control =
    String.exists
      (fun c ->
        let code = Char.code c in
        code < 0x20 || code = 0x7F)
      url
  in
  url <> "" && (not (String.starts_with ~prefix:"#" url)) && not has_control

let hyperlink url text =
  if not (url_is_valid url) then text
  else
    let open Charm_ansi in
    let token = Link.osc8 (Some ({ url; params = [] } : Link.t)) in
    let close = Link.osc8 None in
    token ^ text ^ close

let replace_emoji s =
  let rec find_close i =
    if i >= String.length s then None
    else if s.[i] = ':' then Some i
    else find_close (i + 1)
  in
  let rec loop i b =
    if i >= String.length s then Buffer.contents b
    else if s.[i] <> ':' then (
      Buffer.add_char b s.[i];
      loop (i + 1) b)
    else
      match find_close (i + 1) with
      | None ->
          Buffer.add_substring b s i (String.length s - i);
          Buffer.contents b
      | Some j when j = i + 1 ->
          Buffer.add_char b ':';
          loop (i + 1) b
      | Some j -> (
          let key = String.sub s i (j - i + 1) in
          match List.assoc_opt key Emoji.table with
          | Some value ->
              Buffer.add_string b value;
              loop (j + 1) b
          | None ->
              Buffer.add_char b ':';
              loop (i + 1) b)
  in
  loop 0 (Buffer.create (String.length s))

let plain s = Charm_ansi.Text.strip s

let inline_text ctx text =
  let text = if ctx.emoji then replace_emoji text else text in
  style_text ctx.theme.Theme.text text

let link_destination ctx link =
  match Cmarkit.Inline.Link.reference_definition ctx.defs link with
  | Some (Cmarkit.Link_definition.Def (definition, _)) ->
      Option.map fst (Cmarkit.Link_definition.dest definition)
  | Some (Cmarkit.Block.Footnote.Def (footnote, _)) ->
      Some ("#" ^ Cmarkit.Label.key (Cmarkit.Block.Footnote.label footnote))
  | Some _ -> None
  | None -> None

let footnote_of_link ctx link =
  match Cmarkit.Inline.Link.reference_definition ctx.defs link with
  | Some (Cmarkit.Block.Footnote.Def (fn, _)) -> Some fn
  | _ -> None

let footnote_number ctx fn =
  let key = Cmarkit.Label.key (Cmarkit.Block.Footnote.label fn) in
  let rec find n = function
    | [] -> None
    | (k, _) :: rest -> if k = key then Some n else find (n + 1) rest
  in
  find 1 ctx.footnotes

let add_table_link ctx item =
  if
    not
      (List.exists
         (fun x -> x.href = item.href && x.content = item.content && x.image = item.image)
         ctx.table_links)
  then ctx.table_links <- ctx.table_links @ [ item ]

let rec render_inline ctx (inline : Cmarkit.Inline.t) =
  match inline with
  | Cmarkit.Inline.Text (text, _) -> inline_text ctx text
  | Cmarkit.Inline.Inlines (items, _) ->
      String.concat "" (List.map (render_inline ctx) items)
  | Cmarkit.Inline.Break (br, _) -> (
      match Cmarkit.Inline.Break.type' br with
      | `Hard -> "\n"
      | `Soft -> if ctx.preserve_newlines then "\n" else " ")
  | Cmarkit.Inline.Code_span (code, _) ->
      let b = ctx.theme.Theme.code in
      style_text b (Cmarkit.Inline.Code_span.code code)
  | Cmarkit.Inline.Emphasis (emph, _) ->
      style_text ctx.theme.Theme.emph
        (render_inline ctx (Cmarkit.Inline.Emphasis.inline emph))
  | Cmarkit.Inline.Strong_emphasis (strong, _) ->
      style_text ctx.theme.Theme.strong
        (render_inline ctx (Cmarkit.Inline.Emphasis.inline strong))
  | Cmarkit.Inline.Ext_strikethrough (strike, _) ->
      style_text ctx.theme.Theme.strike
        (render_inline ctx (Cmarkit.Inline.Strikethrough.inline strike))
  | Cmarkit.Inline.Raw_html (lines, _) ->
      let text = String.concat "\n" (List.map Cmarkit.Block_line.tight_to_string lines) in
      style_text ({ ctx.theme.Theme.html_span with faint = true } : Theme.block) text
  | Cmarkit.Inline.Ext_math_span (math, _) ->
      style_text ctx.theme.Theme.code (Cmarkit.Inline.Math_span.tex math)
  | Cmarkit.Inline.Autolink (autolink, _) ->
      let raw_url = fst (Cmarkit.Inline.Autolink.link autolink) in
      let is_email = Cmarkit.Inline.Autolink.is_email autolink in
      let url =
        if is_email && not (String.starts_with ~prefix:"mailto:" raw_url) then
          "mailto:" ^ raw_url
        else raw_url
      in
      let resolved = resolve_url ~base_url:ctx.base_url url in
      let label =
        if is_email then url else Option.value (uri_host resolved) ~default:"link"
      in
      if ctx.in_table then begin
        add_table_link ctx { content = label; href = resolved; image = false };
        style_text ctx.theme.Theme.link_text label
        ^ "["
        ^ string_of_int (List.length ctx.table_links)
        ^ "]"
      end
      else if is_email then style_text ctx.theme.Theme.link_text label
      else hyperlink resolved (style_text ctx.theme.Theme.link resolved)
  | Cmarkit.Inline.Link (link, _) -> (
      let text = render_inline ctx (Cmarkit.Inline.Link.text link) in
      match footnote_of_link ctx link with
      | Some fn -> (
          match footnote_number ctx fn with
          | Some n ->
              let key = Cmarkit.Label.key (Cmarkit.Block.Footnote.label fn) in
              if not (List.mem key ctx.used_footnotes) then
                ctx.used_footnotes <- ctx.used_footnotes @ [ key ];
              "[" ^ string_of_int n ^ "]"
          | None -> text)
      | None ->
          let href = Option.value (link_destination ctx link) ~default:"" in
          let resolved = resolve_url ~base_url:ctx.base_url href in
          if ctx.in_table then begin
            add_table_link ctx { content = plain text; href = resolved; image = false };
            style_text ctx.theme.Theme.link_text text
            ^ "["
            ^ string_of_int (List.length ctx.table_links)
            ^ "]"
          end
          else
            let text = style_text ctx.theme.Theme.link_text text in
            if url_is_valid resolved then
              hyperlink resolved text ^ " ("
              ^ hyperlink resolved (style_text ctx.theme.Theme.link resolved)
              ^ ")"
            else text)
  | Cmarkit.Inline.Image (image, _) ->
      let alt = render_inline ctx (Cmarkit.Inline.Link.text image) |> plain in
      let href = Option.value (link_destination ctx image) ~default:"" in
      let resolved = resolve_url ~base_url:ctx.base_url href in
      let alt =
        if alt = "" then Option.value (uri_host resolved) ~default:"Image" else alt
      in
      if ctx.in_table then begin
        add_table_link ctx { content = alt; href = resolved; image = true };
        style_text ctx.theme.Theme.image_text alt
        ^ "["
        ^ string_of_int (List.length ctx.table_links)
        ^ "]"
      end
      else
        let text = style_text ctx.theme.Theme.image_text alt in
        if url_is_valid resolved then
          text ^ " " ^ hyperlink resolved (style_text ctx.theme.Theme.image resolved)
        else text
  | _ -> invalid_arg "charm.glamour: unsupported inline extension"

let merge_block ?(inherit_affixes = false) (parent : Theme.block) (child : Theme.block) :
    Theme.block =
  let choose_text a b = if b = "" then a else b in
  {
    prefix =
      (if inherit_affixes then choose_text parent.Theme.prefix child.Theme.prefix
       else child.Theme.prefix);
    suffix =
      (if inherit_affixes then choose_text parent.Theme.suffix child.Theme.suffix
       else child.Theme.suffix);
    indent = (if child.Theme.indent = 0 then parent.Theme.indent else child.Theme.indent);
    margin = (if child.Theme.margin = 0 then parent.Theme.margin else child.Theme.margin);
    color = (match child.Theme.color with Some _ as c -> c | None -> parent.Theme.color);
    background =
      (match child.Theme.background with
      | Some _ as b -> b
      | None -> parent.Theme.background);
    bold = parent.Theme.bold || child.Theme.bold;
    italic = parent.Theme.italic || child.Theme.italic;
    underline = parent.Theme.underline || child.Theme.underline;
    faint = parent.Theme.faint || child.Theme.faint;
    strike = parent.Theme.strike || child.Theme.strike;
    block_prefix =
      (if inherit_affixes then
         choose_text parent.Theme.block_prefix child.Theme.block_prefix
       else child.Theme.block_prefix);
    block_suffix =
      (if inherit_affixes then
         choose_text parent.Theme.block_suffix child.Theme.block_suffix
       else child.Theme.block_suffix);
  }

let render_wrapped ctx (b : Theme.block) ~indent content =
  let available =
    if ctx.width < 1 then 0 else max 1 (ctx.width - indent - (2 * b.Theme.margin))
  in
  let styled = style_text b (b.Theme.prefix ^ content ^ b.Theme.suffix) in
  let wrapped = wrap_text available styled in
  b.Theme.block_prefix ^ wrapped ^ b.Theme.block_suffix

let render_paragraph ctx ~indent paragraph =
  let content = render_inline ctx (Cmarkit.Block.Paragraph.inline paragraph) in
  let content =
    if ctx.preserve_newlines then content
    else String.map (fun c -> if c = '\n' then ' ' else c) content
  in
  let visible = plain content in
  let is_description = String.starts_with ~prefix:": " visible in
  let content =
    if is_description then String.sub content 2 (String.length content - 2) else content
  in
  let style =
    if is_description then ctx.theme.Theme.definition_description
    else merge_block ctx.theme.Theme.document ctx.theme.Theme.paragraph
  in
  render_wrapped ctx style ~indent content

let heading_style (theme : Theme.t) level : Theme.block =
  let level_style =
    match level with
    | 1 -> theme.Theme.h1
    | 2 -> theme.Theme.h2
    | 3 -> theme.Theme.h3
    | 4 -> theme.Theme.h4
    | 5 -> theme.Theme.h5
    | _ -> theme.Theme.h6
  in
  let merged : Theme.block =
    merge_block ~inherit_affixes:true theme.Theme.heading level_style
  in
  if level = 6 && not level_style.Theme.bold then
    ({ merged with bold = false } : Theme.block)
  else merged

let render_heading ctx ~indent heading =
  let level = Cmarkit.Block.Heading.level heading in
  let content = render_inline ctx (Cmarkit.Block.Heading.inline heading) in
  render_wrapped ctx (heading_style ctx.theme level) ~indent content

let code_text code = String.concat "\n" (List.map Cmarkit.Block_line.to_string code)

let render_code_block ctx ~indent code_block =
  let code = code_text (Cmarkit.Block.Code_block.code code_block) in
  let language =
    Option.bind (Cmarkit.Block.Code_block.info_string code_block) (fun (info, _) ->
        Option.map fst
          (Cmarkit.Block.Code_block.language_of_info_string (String.trim info)))
  in
  let block, code_theme = ctx.theme.Theme.code_block in
  let highlighted =
    match language with
    | Some lang -> (
        match Charm_highlight.find lang with
        | Some spec -> Charm_highlight.render ~theme:code_theme spec code
        | None -> style_text block code)
    | None -> style_text block code
  in
  let left = indent + block.Theme.margin in
  let lines = split_lines highlighted in
  String.concat "\n" (List.map (fun line -> spaces left ^ line) lines)

let rec render_blocks ctx ~indent blocks =
  blocks
  |> List.filter_map (fun block ->
      let rendered = render_block ctx ~indent block in
      if rendered = "" then None else Some rendered)
  |> String.concat "\n\n"

and render_list_item ctx ~indent ~marker_width ~level_indent item =
  let blocks = Cmarkit.Block.List_item.block item in
  match blocks with
  | Cmarkit.Block.Blocks (children, _) ->
      let rec loop acc = function
        | [] -> List.rev acc
        | block :: rest ->
            let rendered =
              match block with
              | Cmarkit.Block.Paragraph (paragraph, _) ->
                  render_paragraph ctx ~indent:(indent + marker_width) paragraph
              | Cmarkit.Block.List (list, _) ->
                  render_list ctx ~indent:(indent + marker_width + level_indent) list
              | _ -> render_block ctx ~indent:(indent + marker_width) block
            in
            loop (rendered :: acc) rest
      in
      String.concat "\n" (loop [] children)
  | _ -> render_block ctx ~indent:(indent + marker_width) blocks

and render_list ctx ~indent list =
  let ordered, start =
    match Cmarkit.Block.List'.type' list with
    | `Ordered (start, _) -> (true, start)
    | `Unordered _ -> (false, 1)
  in
  let _, level_indent = ctx.theme.Theme.list in
  let items = Cmarkit.Block.List'.items list in
  items
  |> List.mapi (fun index (item, _) ->
      let task_marker =
        Option.bind (Cmarkit.Block.List_item.ext_task_marker item) (fun (u, _) ->
            match Cmarkit.Block.List_item.task_status_of_task_marker u with
            | `Checked -> Some ctx.theme.Theme.task_ticked
            | `Unchecked -> Some ctx.theme.Theme.task_unticked
            | `Cancelled -> Some ctx.theme.Theme.task_unticked
            | `Other _ -> Some ctx.theme.Theme.task_unticked)
      in
      let marker =
        match task_marker with
        | Some marker -> style_text Theme.empty marker
        | None ->
            if ordered then
              style_text
                (Theme.block ~block_prefix:ctx.theme.Theme.enumeration ())
                (string_of_int (start + index) ^ ctx.theme.Theme.enumeration)
            else
              style_text
                (Theme.block ~block_prefix:ctx.theme.Theme.item ())
                ctx.theme.Theme.item
      in
      let marker_width = max 1 (Charm_ansi.Text.width marker) in
      let body = render_list_item ctx ~indent ~marker_width ~level_indent item in
      let lines = split_lines body in
      let first_line = match lines with [] -> "" | x :: _ -> x in
      let rest = match lines with [] | [ _ ] -> [] | _ :: xs -> xs in
      let first = spaces indent ^ marker ^ first_line in
      let continuation =
        List.map (fun line -> spaces (indent + marker_width) ^ line) rest
      in
      String.concat "\n" (first :: continuation))
  |> String.concat "\n"

and render_block ctx ~indent block =
  match block with
  | Cmarkit.Block.Blank_line _ -> ""
  | Cmarkit.Block.Blocks (blocks, _) -> render_blocks ctx ~indent blocks
  | Cmarkit.Block.Paragraph (paragraph, _) -> render_paragraph ctx ~indent paragraph
  | Cmarkit.Block.Heading (heading, _) -> render_heading ctx ~indent heading
  | Cmarkit.Block.Block_quote (quote, _) ->
      let marker = ctx.theme.Theme.block_quote.Theme.block_prefix in
      let marker_width =
        if marker = "" then max 1 ctx.theme.Theme.block_quote.Theme.indent
        else Charm_ansi.Text.width marker
      in
      let rendered =
        render_block ctx ~indent:(indent + marker_width)
          (Cmarkit.Block.Block_quote.block quote)
      in
      let marker = if marker = "" then spaces marker_width else marker in
      rendered |> split_lines
      |> List.map (fun line ->
          spaces indent ^ marker ^ style_text ctx.theme.Theme.block_quote line)
      |> String.concat "\n"
  | Cmarkit.Block.List (list, _) -> render_list ctx ~indent list
  | Cmarkit.Block.Code_block (code, _) -> render_code_block ctx ~indent code
  | Cmarkit.Block.Thematic_break _ ->
      let b = ctx.theme.Theme.hr in
      style_text b (if b.Theme.prefix <> "" then b.Theme.prefix else "--------")
  | Cmarkit.Block.Html_block (lines, _) ->
      let text = String.concat "\n" (List.map Cmarkit.Block_line.to_string lines) in
      style_text ({ ctx.theme.Theme.html_block with faint = true } : Theme.block) text
  | Cmarkit.Block.Link_reference_definition _ -> ""
  | Cmarkit.Block.Ext_math_block (code, _) ->
      style_text
        (fst ctx.theme.Theme.code_block)
        (code_text (Cmarkit.Block.Code_block.code code))
  | Cmarkit.Block.Ext_table (table, _) -> render_table ctx ~indent table
  | Cmarkit.Block.Ext_footnote_definition _ -> ""
  | _ -> invalid_arg "charm.glamour: unsupported block extension"

and render_table ctx ~indent table =
  let old_in_table = ctx.in_table in
  let old_links = ctx.table_links in
  ctx.in_table <- true;
  ctx.table_links <- [];
  let headers = ref [] and rows = ref [] and alignments = ref [] in
  let add_cells cells = List.map (fun (inline, _) -> render_inline ctx inline) cells in
  List.iter
    (fun ((row, _), _) ->
      match row with
      | `Header cells -> headers := add_cells cells
      | `Data cells -> rows := !rows @ [ add_cells cells ]
      | `Sep cells -> alignments := List.map (fun ((alignment, _), _) -> alignment) cells)
    (Cmarkit.Block.Table.rows table);
  let table_block, separator = ctx.theme.Theme.table in
  let border =
    if separator = "|" then Charm_lipgloss.Border.ascii
    else if separator = "│" || separator = "" then Charm_lipgloss.Border.normal
    else
      {
        Charm_lipgloss.Border.top = separator;
        bottom = separator;
        left = separator;
        right = separator;
        top_left = separator;
        top_right = separator;
        bottom_left = separator;
        bottom_right = separator;
        middle_left = separator;
        middle_right = separator;
        middle = separator;
        middle_top = separator;
        middle_bottom = separator;
      }
  in
  let style ~row:_ ~col =
    let style =
      style_of_block table_block
      |> Charm_lipgloss.Style.inline false
      |> Charm_lipgloss.Style.margin (Charm_lipgloss.Sides.xy ~x:1 ~y:0)
    in
    match List.nth_opt !alignments col with
    | Some (Some `Left) ->
        Charm_lipgloss.Style.align_horizontal Charm_lipgloss.Position.left style
    | Some (Some `Center) ->
        Charm_lipgloss.Style.align_horizontal Charm_lipgloss.Position.center style
    | Some (Some `Right) ->
        Charm_lipgloss.Style.align_horizontal Charm_lipgloss.Position.right style
    | _ -> style
  in
  let width =
    if ctx.width < 1 then None
    else Some (max 1 (ctx.width - indent - (2 * table_block.Theme.margin)))
  in
  let headers, rows =
    match (width, ctx.table_wrap) with
    | Some width, false ->
        let columns =
          max (List.length !headers)
            (List.fold_left (fun count row -> max count (List.length row)) 0 !rows)
        in
        let budget = max 1 ((width / max 1 columns) - 3) in
        let truncate cell =
          if Charm_ansi.Text.width cell > budget then
            Charm_ansi.Text.truncate ~width:budget ~tail:"…" cell
          else cell
        in
        (List.map truncate !headers, List.map (List.map truncate) !rows)
    | _ -> (!headers, !rows)
  in
  let tbl =
    match width with
    | None -> Charm_lipgloss.Table.v ~headers ~rows ~border ~style ~wrap:ctx.table_wrap ()
    | Some width ->
        Charm_lipgloss.Table.v ~headers ~rows ~border ~style ~width ~wrap:ctx.table_wrap
          ()
  in
  let rendered = Charm_lipgloss.Table.render tbl in
  let rendered =
    table_block.Theme.block_prefix
    ^ style_text table_block
        (table_block.Theme.prefix ^ rendered ^ table_block.Theme.suffix)
    ^ table_block.Theme.block_suffix
  in
  let body =
    String.concat "\n"
      (List.map (fun line -> spaces indent ^ line) (split_lines rendered))
  in
  let footer =
    ctx.table_links
    |> List.mapi (fun i link ->
        let href = resolve_url ~base_url:ctx.base_url link.href in
        let label = link.content in
        Fmt.str "[%d]: %s%s" (i + 1) label
          (if href = "" then ""
           else " " ^ hyperlink href (style_text ctx.theme.Theme.link href)))
    |> String.concat "\n"
  in
  ctx.in_table <- old_in_table;
  ctx.table_links <- old_links;
  if footer = "" then body else body ^ "\n\n" ^ footer

let collect_footnotes defs =
  Cmarkit.Label.Map.bindings defs
  |> List.filter_map (fun (key, def) ->
      match def with Cmarkit.Block.Footnote.Def (fn, _) -> Some (key, fn) | _ -> None)

let render ?(width = 80) ?(theme = (Theme.dark : Theme.t)) ?(base_url = "")
    ?(preserve_newlines = false) ?(emoji = false) ?(table_wrap = true) markdown =
  let doc = Cmarkit.Doc.of_string ~strict:false ~layout:false ~locs:false markdown in
  let block = Cmarkit.Block.normalize (Cmarkit.Doc.block doc) in
  let defs = Cmarkit.Doc.defs doc in
  let ctx =
    {
      width;
      theme;
      base_url;
      preserve_newlines;
      emoji;
      table_wrap;
      defs;
      footnotes = collect_footnotes defs;
      used_footnotes = [];
      in_table = false;
      table_links = [];
    }
  in
  let body = render_block ctx ~indent:0 block in
  let footers =
    ctx.used_footnotes
    |> List.filter_map (fun key ->
        Option.map
          (fun fn ->
            let number = Option.value (footnote_number ctx fn) ~default:1 in
            let content =
              render_block ctx ~indent:4 (Cmarkit.Block.Footnote.block fn) |> plain
            in
            Fmt.str "[%d]: %s" number content)
          (List.assoc_opt key ctx.footnotes))
  in
  let body = if footers = [] then body else body ^ "\n\n" ^ String.concat "\n" footers in
  let document = (theme : Theme.t).Theme.document in
  let body =
    if body = "" then ""
    else
      document.Theme.block_prefix ^ style_text document body ^ document.Theme.block_suffix
  in
  if document.Theme.margin <= 0 then body
  else
    body |> split_lines
    |> List.map (fun line ->
        if line = "" then line else spaces document.Theme.margin ^ line)
    |> String.concat "\n"
