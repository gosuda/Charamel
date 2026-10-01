let fill ?(pattern = " ") n =
  if n <= 0 then ""
  else
    let pattern = if pattern = "" then " " else pattern in
    let glyphs = Charamel_ansi.Width.graphemes pattern in
    let count = Stdlib.List.length glyphs in
    let out = Buffer.create n in
    let rec loop i remaining =
      if remaining <= 0 then ()
      else
        let g = Stdlib.List.nth glyphs (i mod count) in
        let w = max 1 (Charamel_ansi.Width.grapheme_width g) in
        if w <= remaining then begin
          Buffer.add_string out g;
          loop (i + 1) (remaining - w)
        end
        else Buffer.add_string out (String.make remaining ' ')
    in
    loop 0 n;
    let contents = Buffer.contents out in
    let short = n - Charamel_ansi.Text.width contents in
    if short > 0 then contents ^ String.make short ' ' else contents
