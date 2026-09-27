type state = Ground | Escape | Csi | Dcs | String_payload of bool
type token = Sequence of string | Glyph of string * int

let scalar_length s i =
  let decoded = String.get_utf_8_uchar s i in
  if Uchar.utf_decode_is_valid decoded then Uchar.utf_decode_length decoded else 1

let fold_spans emit s =
  let state = ref Ground in
  let start = ref 0 in
  let raw = ref true in
  let i = ref 0 in
  let flush stop = if stop > !start then emit !raw !start stop in
  while !i < String.length s do
    let b = String.get_uint8 s !i in
    let count = if b >= 0xc2 then scalar_length s !i else 1 in
    let next_raw, next_state =
      match !state with
      | String_payload osc ->
          if count > 1 then (false, !state)
          else if b = 0x1b then (false, Escape)
          else if b = 0x9c || b = 0x18 || b = 0x1a || (osc && b = 7) then (false, Ground)
          else (false, !state)
      | _ when b = 0x1b -> (false, Escape)
      | _ when count > 1 -> (true, Ground)
      | _ when b = 0x9b -> (false, Csi)
      | _ when b = 0x90 -> (false, Dcs)
      | _ when b = 0x9d -> (false, String_payload true)
      | _ when b = 0x98 || b = 0x9e || b = 0x9f -> (false, String_payload false)
      | _ when b = 0x18 || b = 0x1a -> (true, Ground)
      | Ground -> (true, Ground)
      | Escape ->
          if b = 0x5b then (false, Csi)
          else if b = 0x50 then (false, Dcs)
          else if b = 0x5d then (false, String_payload true)
          else if b = 0x58 || b = 0x5e || b = 0x5f then (false, String_payload false)
          else if b < 0x20 then (true, Escape)
          else if b >= 0x30 && b <= 0x7e then (false, Ground)
          else (false, Escape)
      | Csi ->
          if b < 0x20 then (true, Csi)
          else if b >= 0x40 && b <= 0x7e then (false, Ground)
          else (false, Csi)
      | Dcs ->
          if b >= 0x40 && b <= 0x7e then (false, String_payload false) else (false, Dcs)
    in
    if next_raw <> !raw then begin
      flush !i;
      start := !i;
      raw := next_raw
    end;
    state := next_state;
    i := !i + count
  done;
  flush !i

let fold_tokens emit s =
  fold_spans
    (fun raw first last ->
      let text = String.sub s first (last - first) in
      if raw then
        Uuseg_string.fold_utf_8 `Grapheme_cluster
          (fun () g ->
            if g = "\r\n" then begin
              emit (Glyph ("\r", 0));
              emit (Glyph ("\n", 0))
            end
            else emit (Glyph (g, Width.grapheme_width g)))
          () text
      else emit (Sequence text))
    s

let strip s =
  let out = Buffer.create (String.length s) in
  fold_spans
    (fun raw first last -> if raw then Buffer.add_substring out s first (last - first))
    s;
  Buffer.contents out

let width s = Width.string_width (strip s)
let first_scalar s = Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0)
let is_space s = s <> "" && Uucp.White.is_white_space (first_scalar s)
let is_nbsp s = s <> "" && Uchar.to_int (first_scalar s) = 0xa0

let truncate ?(tail = "") ~width:max_width s =
  if width s <= max_width then s
  else
    let budget = max_width - width tail in
    if budget < 0 then ""
    else
      let out = Buffer.create (String.length s) in
      let column = ref 0 in
      let stopped = ref false in
      fold_tokens
        (function
          | Sequence seq -> Buffer.add_string out seq
          | Glyph (g, w) ->
              if not !stopped then begin
                if !column + w > budget then begin
                  Buffer.add_string out tail;
                  stopped := true
                end
                else Buffer.add_string out g;
                column := !column + w
              end)
        s;
      Buffer.contents out

let truncate_left ?(prefix = "") ~width:count s =
  if count <= 0 then s
  else
    let out = Buffer.create (String.length s) in
    let column = ref 0 in
    let keeping = ref false in
    fold_tokens
      (function
        | Sequence seq -> Buffer.add_string out seq
        | Glyph (g, w) ->
            column := !column + w;
            if (not !keeping) && !column > count then begin
              Buffer.add_string out prefix;
              keeping := true
            end;
            if !keeping then Buffer.add_string out g)
      s;
    Buffer.contents out

let cut ~left ~right s =
  if right <= left then "" else truncate_left ~width:left (truncate ~width:right s)

let hardwrap ?(preserve_space = false) ~width:limit s =
  if limit < 1 then s
  else
    let out = Buffer.create (String.length s) in
    let column = ref 0 in
    let wrapped = ref false in
    let newline () =
      Buffer.add_char out '\n';
      column := 0
    in
    fold_tokens
      (function
        | Sequence seq -> Buffer.add_string out seq
        | Glyph ("\n", _) ->
            newline ();
            wrapped := false
        | Glyph (g, w) ->
            let required =
              if String.length g = 1 && Char.code g.[0] < 0x20 then 1 else w
            in
            if !column > 0 && !column + required > limit then begin
              newline ();
              wrapped := true
            end;
            let skip = (not preserve_space) && !column = 0 && !wrapped && is_space g in
            if not skip then begin
              Buffer.add_string out g;
              column := !column + w;
              wrapped := false
            end)
      s;
    Buffer.contents out

let break_chars s =
  let len = String.length s in
  let rec loop chars i =
    if i >= len then chars
    else
      let decoded = String.get_utf_8_uchar s i in
      let chars =
        if Uchar.utf_decode_is_valid decoded then Uchar.utf_decode_uchar decoded :: chars
        else chars
      in
      loop chars (i + Uchar.utf_decode_length decoded)
  in
  loop [] 0

type word_policy = Keep_words | Split_words

let wrap_words policy ~breakpoints ~limit s =
  if limit < 1 then s
  else
    let out = Buffer.create (String.length s) in
    let word = Buffer.create 32 in
    let space = Buffer.create 8 in
    let column = ref 0 in
    let word_width = ref 0 in
    let space_width = ref 0 in
    let breakpoints = Uchar.of_int 0x2d :: break_chars breakpoints in
    let add_space () =
      if !word_width > 0 || !column + !space_width <= limit then begin
        Buffer.add_buffer out space;
        column := !column + !space_width
      end;
      Buffer.clear space;
      space_width := 0
    in
    let add_word () =
      if Buffer.length word > 0 then begin
        add_space ();
        Buffer.add_buffer out word;
        column := !column + !word_width;
        Buffer.clear word;
        word_width := 0
      end
    in
    let newline () =
      Buffer.add_char out '\n';
      column := 0;
      Buffer.clear space;
      space_width := 0
    in
    let append_word g w =
      if policy = Split_words && !word_width + w > limit then add_word ();
      Buffer.add_string word g;
      word_width := !word_width + w;
      let over_wide_at_line_start =
        !column = 0 && !space_width = 0 && !word_width > limit
      in
      if
        !column + !word_width + !space_width > limit
        && (policy = Split_words || !word_width < limit)
        && not over_wide_at_line_start
      then newline ();
      if policy = Split_words && !word_width = limit then add_word ()
    in
    fold_tokens
      (function
        | Sequence seq -> Buffer.add_string word seq
        | Glyph ("\n", _) ->
            if !word_width = 0 then begin
              if !column + !space_width <= limit then Buffer.add_buffer out space;
              Buffer.clear space;
              space_width := 0
            end;
            add_word ();
            newline ()
        | Glyph (g, w) ->
            if is_space g && not (is_nbsp g) then begin
              add_word ();
              Buffer.add_string space g;
              space_width := !space_width + w
            end
            else if List.exists (Uchar.equal (first_scalar g)) breakpoints then begin
              add_space ();
              if policy = Split_words && !column + !word_width + w > limit then begin
                Buffer.add_string word g;
                word_width := !word_width + w
              end
              else begin
                add_word ();
                Buffer.add_string out g;
                column := !column + w
              end
            end
            else append_word g w)
      s;
    if policy = Split_words && !word_width = 0 then begin
      if !column + !space_width <= limit then Buffer.add_buffer out space;
      Buffer.clear space;
      space_width := 0
    end;
    add_word ();
    Buffer.contents out

let wordwrap ?(breakpoints = "") ~width s =
  wrap_words Keep_words ~breakpoints ~limit:width s

let wrap ?(breakpoints = "") ~width s = wrap_words Split_words ~breakpoints ~limit:width s
let pad_right ~width:target s = s ^ String.make (max 0 (target - width s)) ' '
