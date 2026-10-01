type rune_index = [ `Grapheme | `Scalar ]

let width s =
  Stdlib.List.fold_left
    (fun m line -> max m (Charamel_ansi.Text.width line))
    0 (String.split_on_char '\n' s)

let height s = Stdlib.List.length (String.split_on_char '\n' s)
let size s = (width s, height s)
let lines s = String.split_on_char '\n' s
let spaces n = String.make (max 0 n) ' '

let join_horizontal ?(pos = Position.top) blocks =
  match blocks with
  | [] -> ""
  | [ x ] -> x
  | _ ->
      let split = Stdlib.List.map (fun s -> (lines s, width s)) blocks in
      let h =
        Stdlib.List.fold_left (fun n (ls, _) -> max n (Stdlib.List.length ls)) 0 split
      in
      let padded =
        Stdlib.List.map
          (fun (ls, w) ->
            let missing = h - Stdlib.List.length ls in
            let top, bottom = Position.split pos missing in
            ( Stdlib.List.init top (fun _ -> "")
              @ ls
              @ Stdlib.List.init bottom (fun _ -> ""),
              w ))
          split
      in
      Stdlib.List.init h (fun i ->
          Stdlib.List.fold_left
            (fun acc (ls, w) ->
              let line = Stdlib.List.nth ls i in
              acc ^ line ^ spaces (w - Charamel_ansi.Text.width line))
            "" padded)
      |> String.concat "\n"

let join_vertical ?(pos = Position.left) blocks =
  match blocks with
  | [] -> ""
  | [ x ] -> x
  | _ ->
      let split = Stdlib.List.map (fun s -> (lines s, width s)) blocks in
      let widest = Stdlib.List.fold_left (fun n (_, w) -> max n w) 0 split in
      let result =
        Stdlib.List.concat_map
          (fun (ls, _) ->
            Stdlib.List.map
              (fun line ->
                let gap = widest - Charamel_ansi.Text.width line in
                if gap <= 0 then line
                else
                  let left, right = Position.split pos gap in
                  spaces left ^ line ^ spaces right)
              ls)
          split
      in
      String.concat "\n" result

let fill ?(whitespace = (" ", Style.empty)) n =
  let chars, style = whitespace in
  if n <= 0 then "" else Style.render style (Whitespace.fill ~pattern:chars n)

let place_horizontal ?(whitespace = (" ", Style.empty)) ~width:target ~pos text =
  let ls = lines text in
  let current = width text in
  if target <= current then text
  else
    Stdlib.List.map
      (fun line ->
        let gap = target - Charamel_ansi.Text.width line in
        if pos = Position.left then line ^ fill ~whitespace gap
        else if pos = Position.right then fill ~whitespace gap ^ line
        else
          let share, rest = Position.split pos gap in
          fill ~whitespace rest ^ line ^ fill ~whitespace share)
      ls
    |> String.concat "\n"

let place_vertical ?(whitespace = (" ", Style.empty)) ~height:target ~pos text =
  let ls = lines text in
  let current = Stdlib.List.length ls in
  if target <= current then text
  else
    let gap = target - current in
    let top, bottom =
      if pos = Position.bottom then (gap, 0)
      else if pos = Position.top then (0, gap)
      else
        let share, rest = Position.split pos gap in
        (rest, share)
    in
    let empty_line = fill ~whitespace (width text) in
    String.concat "\n"
      (Stdlib.List.init top (fun _ -> empty_line)
      @ ls
      @ Stdlib.List.init bottom (fun _ -> empty_line))

let place ?(h = Position.left) ?(v = Position.top) ?whitespace ~width ~height text =
  let text = place_horizontal ?whitespace ~width ~pos:h text in
  place_vertical ?whitespace ~height ~pos:v text

let identity_style style = Style.render style "" = "" && Style.render style "x" = "x"

let style_piece style ~left ~right ~original ~plain =
  if right <= left then ""
  else if identity_style style then Charamel_ansi.Text.cut ~left ~right original
  else Style.render style (Charamel_ansi.Text.cut ~left ~right plain)

let style_ranges ranges text =
  match ranges with
  | [] -> text
  | _ when Stdlib.List.for_all (fun (_, _, style) -> identity_style style) ranges -> text
  | _ ->
      let plain = Charamel_ansi.Text.strip text in
      let out = Buffer.create (String.length text) in
      let last = ref 0 in
      Stdlib.List.iter
        (fun (start, stop, style) ->
          if start > !last then
            Buffer.add_string out (Charamel_ansi.Text.cut ~left:!last ~right:start text);
          Buffer.add_string out
            (style_piece style ~left:start ~right:stop ~original:text ~plain);
          last := stop)
        ranges;
      Buffer.add_string out (Charamel_ansi.Text.truncate_left ~width:!last text);
      Buffer.contents out

let scalar_count cluster =
  let n = String.length cluster in
  let rec loop i count =
    if i >= n then count
    else
      let decoded = String.get_utf_8_uchar cluster i in
      if Uchar.utf_decode_is_valid decoded then
        loop (i + Uchar.utf_decode_length decoded) (count + 1)
      else loop (i + 1) count
  in
  loop 0 0

let scalar_clusters plain indices =
  let clusters = Charamel_ansi.Width.graphemes plain in
  let scalars =
    Stdlib.List.fold_left (fun n cluster -> n + scalar_count cluster) 0 clusters
  in
  let owner = Array.make scalars (-1) in
  let position = ref 0 in
  Stdlib.List.iteri
    (fun cluster_index cluster ->
      let width = scalar_count cluster in
      for offset = 0 to width - 1 do
        owner.(!position + offset) <- cluster_index
      done;
      position := !position + width)
    clusters;
  Stdlib.List.sort_uniq compare
    (Stdlib.List.filter_map
       (fun index -> if index >= 0 && index < scalars then Some owner.(index) else None)
       indices)

let link_params field =
  Stdlib.List.filter_map
    (fun text ->
      match String.split_on_char '=' text with
      | key :: value when key <> "" -> Some (key, String.concat "=" value)
      | _ -> None)
    (String.split_on_char ':' field)

let wrap ?breakpoints ~width text =
  let source =
    if width <= 1 then text else Charamel_ansi.Text.wrap ?breakpoints ~width text
  in
  let parser = Charamel_ansi.Parser.create () in
  let style = ref Charamel_ansi.Style.default in
  let link = ref (None : Charamel_ansi.Link.t option) in
  let out = Buffer.create (String.length source + 16) in
  let styled () = not (Charamel_ansi.Style.equal !style Charamel_ansi.Style.default) in
  let track chunk =
    Stdlib.List.iter
      (fun action ->
        match action with
        | Charamel_ansi.Parser.Csi { final = 'm'; params; _ } ->
            let params = if params = [] then [ [ Some 0 ] ] else params in
            style := Charamel_ansi.Style.of_sgr ~params !style
        | Charamel_ansi.Parser.Osc ("8" :: parameters :: rest) ->
            let uri = String.concat ";" rest in
            link :=
              if uri = "" then None
              else Some { Charamel_ansi.Link.url = uri; params = link_params parameters }
        | _ -> ())
      (Charamel_ansi.Parser.feed parser chunk)
  in
  let start = ref 0 in
  let emit stop =
    if stop > !start then begin
      let chunk = String.sub source !start (stop - !start) in
      track chunk;
      Buffer.add_string out chunk;
      start := stop
    end
  in
  let close () =
    if styled () then
      Buffer.add_string out (Charamel_ansi.Style.to_sgr Charamel_ansi.Style.default);
    if !link <> None then Buffer.add_string out (Charamel_ansi.Link.osc8 None)
  in
  let reopen () =
    (match !link with
    | Some target -> Buffer.add_string out (Charamel_ansi.Link.osc8 (Some target))
    | None -> ());
    if styled () then Buffer.add_string out (Charamel_ansi.Style.to_sgr !style)
  in
  let n = String.length source in
  for index = 0 to n - 1 do
    if source.[index] = '\n' then begin
      emit index;
      close ();
      Buffer.add_char out '\n';
      reopen ();
      start := index + 1
    end
  done;
  emit n;
  close ();
  Buffer.contents out

let style_runes ?(basis = `Grapheme) matched unmatched text ~indices =
  let plain = Charamel_ansi.Text.strip text in
  let indices =
    match basis with `Grapheme -> indices | `Scalar -> scalar_clusters plain indices
  in
  let glyphs = Charamel_ansi.Width.graphemes plain in
  let runs_rev, _, _ =
    Stdlib.List.fold_left
      (fun (runs, index, cell) glyph ->
        let selected = Stdlib.List.mem index indices in
        let next_cell = cell + max 0 (Charamel_ansi.Width.grapheme_width glyph) in
        let runs =
          if next_cell = cell then runs
          else
            match runs with
            | (left, right, same) :: rest when same = selected && right = cell ->
                (left, next_cell, same) :: rest
            | _ -> (cell, next_cell, selected) :: runs
        in
        (runs, index + 1, next_cell))
      ([], 0, 0) glyphs
  in
  let ranges =
    Stdlib.List.rev_map
      (fun (left, right, selected) ->
        (left, right, if selected then matched else unmatched))
      runs_rev
  in
  style_ranges ranges text
