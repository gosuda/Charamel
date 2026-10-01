type cell = {
  text : string;
  width : int;
  style : Style.t;
  link : Link.t option;
  cont : bool;
}

let link_equal a b =
  match (a, b) with
  | None, None -> true
  | Some (x : Link.t), Some (y : Link.t) ->
      String.equal x.Link.url y.Link.url && x.Link.params = y.Link.params
  | _ -> false

let equal a b =
  a.width = b.width && a.cont = b.cont && String.equal a.text b.text
  && Style.equal a.style b.style && link_equal a.link b.link

let blank = { text = " "; width = 1; style = Style.default; link = None; cont = false }
let is_blank c = equal c blank

let last_nonblank line =
  let rec loop i = if i < 0 then -1 else if is_blank line.(i) then loop (i - 1) else i in
  loop (Array.length line - 1)

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

let layout ~width ~max_rows content =
  if width <= 0 || max_rows <= 0 then [||]
  else begin
    let rows = Dynarray.create () in
    let current = ref (Array.make width blank) in
    let column = ref 0 in
    let style = ref Style.default in
    let link = ref None in
    let full = ref false in
    let finish_line () =
      if not !full then begin
        Dynarray.add_last rows !current;
        if Dynarray.length rows >= max_rows then full := true
      end;
      current := Array.make width blank;
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
    let clear_overlap span =
      if !column < width && !current.(!column).cont then begin
        !current.(!column) <- blank;
        !current.(!column - 1) <- blank
      end
      else if span = 1 && !column + 1 < width && !current.(!column + 1).cont then
        !current.(!column + 1) <- blank
    in
    let rec put text span =
      if !full then ()
      else if span = 0 then append_zero_width text
      else if span > width then ()
      else if !column + span > width then begin
        finish_line ();
        put text span
      end
      else begin
        clear_overlap span;
        let cell = { text; width = span; style = !style; link = !link; cont = false } in
        !current.(!column) <- cell;
        if span = 2 then !current.(!column + 1) <- { cell with text = ""; cont = true };
        column := !column + span
      end
    in
    let tab () = column := min width (((!column / 8) + 1) * 8) in
    let pending = Buffer.create 16 in
    let flush_print () =
      if Buffer.length pending > 0 then begin
        let text = Buffer.contents pending in
        Buffer.clear pending;
        List.iter
          (fun grapheme -> put grapheme (Width.grapheme_width grapheme))
          (Width.graphemes text)
      end
    in
    let handle action =
      match action with
      | Parser.Print text -> Buffer.add_string pending text
      | Parser.Execute '\n' ->
          flush_print ();
          finish_line ()
      | Parser.Execute '\r' ->
          flush_print ();
          column := 0
      | Parser.Execute '\t' ->
          flush_print ();
          tab ()
      | Parser.Execute '\b' ->
          flush_print ();
          column := max 0 (!column - 1)
      | Parser.Execute _ -> flush_print ()
      | Parser.Csi { final = 'm'; params = []; _ } ->
          flush_print ();
          style := Style.of_sgr ~params:[ [ Some 0 ] ] !style
      | Parser.Csi { final = 'm'; params; _ } ->
          flush_print ();
          style := Style.of_sgr ~params !style
      | Parser.Csi _ -> flush_print ()
      | Parser.Osc ("8" :: parameters :: rest) ->
          flush_print ();
          let uri = String.concat ";" rest in
          link :=
            if uri = "" || not (valid_parameter uri) then None
            else Some { Link.url = uri; params = parse_link_params parameters }
      | Parser.Osc _ -> flush_print ()
      | Parser.Esc _ | Parser.Dcs _ | Parser.Apc _ | Parser.Pm _ | Parser.Sos _ ->
          flush_print ()
    in
    let parser = Parser.create () in
    List.iter handle (Parser.feed parser content);
    List.iter handle (Parser.flush parser);
    flush_print ();
    finish_line ();
    Dynarray.to_array rows
  end

let pad_rows grid ~rows ~width =
  if rows <= 0 then [||]
  else
    let have = Array.length grid in
    if have >= rows then Array.sub grid 0 rows
    else Array.append grid (Array.init (rows - have) (fun _ -> Array.make width blank))

let of_string ~width ~rows content =
  pad_rows (layout ~width ~max_rows:rows content) ~rows ~width

let trim_trailing_spaces line =
  let len = String.length line in
  let rec loop i = if i = 0 then 0 else if line.[i - 1] = ' ' then loop (i - 1) else i in
  let keep = loop len in
  if keep = len then line else String.sub line 0 keep

let to_string ?(trim = false) grid =
  let emitted_style = ref Style.default in
  let link_open = ref None in
  let render_cell buf cell =
    if not (link_equal cell.link !link_open) then begin
      Buffer.add_string buf (Link.osc8 cell.link);
      link_open := cell.link
    end;
    if not (Style.equal !emitted_style cell.style) then begin
      Buffer.add_string buf (Style.transition ~from:!emitted_style cell.style);
      emitted_style := cell.style
    end;
    Buffer.add_string buf cell.text
  in
  let render_row line =
    let buf = Buffer.create 64 in
    Array.iter (fun cell -> if not cell.cont then render_cell buf cell) line;
    let painted = Buffer.contents buf in
    if trim then trim_trailing_spaces painted else painted
  in
  let close_pen buf =
    (match !link_open with
    | Some _ -> Buffer.add_string buf (Link.osc8 None)
    | None -> ());
    if not (Style.equal !emitted_style Style.default) then
      Buffer.add_string buf (Style.to_sgr Style.default)
  in
  if Array.length grid = 0 then ""
  else begin
    let buf = Buffer.create 256 in
    Array.iteri
      (fun index line ->
        if index > 0 then Buffer.add_char buf '\n';
        Buffer.add_string buf (render_row line))
      grid;
    close_pen buf;
    Buffer.contents buf
  end
