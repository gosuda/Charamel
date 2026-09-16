let width s =
  Stdlib.List.fold_left
    (fun m line -> max m (Charm_ansi.Text.width line))
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
            let top, bottom =
              if pos = Position.top then (0, missing)
              else if pos = Position.bottom then (missing, 0)
              else
                let cut =
                  int_of_float (Float.round (float missing *. Position.to_float pos))
                in
                (cut, missing - cut)
            in
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
              acc ^ line ^ spaces (w - Charm_ansi.Text.width line))
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
                let gap = widest - Charm_ansi.Text.width line in
                if gap <= 0 then line
                else if pos = Position.left then line ^ spaces gap
                else if pos = Position.right then spaces gap ^ line
                else
                  let left =
                    int_of_float (Float.round (float gap *. Position.to_float pos))
                  in
                  spaces left ^ line ^ spaces (gap - left))
              ls)
          split
      in
      String.concat "\n" result

let fill ?(whitespace = (" ", Style.empty)) n =
  let chars, style = whitespace in
  let chars = if chars = "" then " " else chars in
  if n <= 0 then ""
  else
    let glyphs = Charm_ansi.Width.graphemes chars in
    let rec loop out i remaining =
      if remaining <= 0 then ()
      else
        let g = Stdlib.List.nth glyphs (i mod Stdlib.List.length glyphs) in
        let w = max 1 (Charm_ansi.Width.grapheme_width g) in
        if w <= remaining then begin
          Buffer.add_string out g;
          loop out (i + 1) (remaining - w)
        end
        else Buffer.add_string out (String.make remaining ' ')
    in
    let out = Buffer.create n in
    loop out 0 n;
    let contents = Buffer.contents out in
    let short = n - Charm_ansi.Text.width contents in
    let contents = if short > 0 then contents ^ String.make short ' ' else contents in
    Style.render style contents

let place_horizontal ?(whitespace = (" ", Style.empty)) ~width:target ~pos text =
  let ls = lines text in
  let current = width text in
  if target <= current then text
  else
    Stdlib.List.map
      (fun line ->
        let gap = target - Charm_ansi.Text.width line in
        if pos = Position.left then line ^ fill ~whitespace gap
        else if pos = Position.right then fill ~whitespace gap ^ line
        else
          let right = int_of_float (Float.round (float gap *. Position.to_float pos)) in
          fill ~whitespace (gap - right) ^ line ^ fill ~whitespace right)
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
        let cut = int_of_float (Float.round (float gap *. Position.to_float pos)) in
        (gap - cut, cut)
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
  else if identity_style style then Charm_ansi.Text.cut ~left ~right original
  else Style.render style (Charm_ansi.Text.cut ~left ~right plain)

let style_ranges ranges text =
  match ranges with
  | [] -> text
  | _ when Stdlib.List.for_all (fun (_, _, style) -> identity_style style) ranges -> text
  | _ ->
      let plain = Charm_ansi.Text.strip text in
      let out = Buffer.create (String.length text) in
      let last = ref 0 in
      Stdlib.List.iter
        (fun (start, stop, style) ->
          if start > !last then
            Buffer.add_string out (Charm_ansi.Text.cut ~left:!last ~right:start text);
          Buffer.add_string out
            (style_piece style ~left:start ~right:stop ~original:text ~plain);
          last := stop)
        ranges;
      Buffer.add_string out (Charm_ansi.Text.truncate_left ~width:!last text);
      Buffer.contents out

let style_runes matched unmatched text ~indices =
  let plain = Charm_ansi.Text.strip text in
  let glyphs = Charm_ansi.Width.graphemes plain in
  let runs_rev, _, _ =
    Stdlib.List.fold_left
      (fun (runs, index, cell) glyph ->
        let selected = Stdlib.List.mem index indices in
        let next_cell = cell + max 0 (Charm_ansi.Width.grapheme_width glyph) in
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
