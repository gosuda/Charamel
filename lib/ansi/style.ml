type underline = No_underline | Single | Double | Curly | Dotted | Dashed

type t = {
  fg : Color.t;
  bg : Color.t;
  underline_color : Color.t;
  bold : bool;
  faint : bool;
  italic : bool;
  underline : underline;
  blink : bool;
  reverse : bool;
  conceal : bool;
  strike : bool;
}

let default =
  {
    fg = Color.Default;
    bg = Color.Default;
    underline_color = Color.Default;
    bold = false;
    faint = false;
    italic = false;
    underline = No_underline;
    blink = false;
    reverse = false;
    conceal = false;
    strike = false;
  }

let equal = ( = )

let is_zero t =
  match (t.fg, t.bg, t.underline_color, t.underline) with
  | Color.Default, Color.Default, Color.Default, No_underline ->
      (not t.bold) && (not t.faint) && (not t.italic) && (not t.blink) && (not t.reverse)
      && (not t.conceal) && not t.strike
  | _ -> false

let in_byte_range v = if v < 0 then 0 else if v > 255 then 255 else v
let in_basic_range n = if n < 0 then 0 else if n > 15 then 15 else n
let in_256_range n = if n < 0 then 0 else if n > 255 then 255 else n

(* The SGR parameter layout follows the emitters of
   .references/x/ansi/style.go; the minimal delta in [transition] follows
   StyleDiff in .references/ultraviolet/cell.go, minus its rapid-blink
   attribute, which this record does not carry. *)
let fg_param = function
  | Color.Default -> "39"
  | Color.Basic n ->
      let n = in_basic_range n in
      if n < 8 then string_of_int (30 + n) else string_of_int (82 + n)
  | Color.Indexed n -> "38;5;" ^ string_of_int (in_256_range n)
  | Color.Rgb (r, g, b) ->
      "38;2;"
      ^ string_of_int (in_byte_range r)
      ^ ";"
      ^ string_of_int (in_byte_range g)
      ^ ";"
      ^ string_of_int (in_byte_range b)

let bg_param = function
  | Color.Default -> "49"
  | Color.Basic n ->
      let n = in_basic_range n in
      if n < 8 then string_of_int (40 + n) else string_of_int (92 + n)
  | Color.Indexed n -> "48;5;" ^ string_of_int (in_256_range n)
  | Color.Rgb (r, g, b) ->
      "48;2;"
      ^ string_of_int (in_byte_range r)
      ^ ";"
      ^ string_of_int (in_byte_range g)
      ^ ";"
      ^ string_of_int (in_byte_range b)

let underline_color_param = function
  | Color.Default -> "59"
  | Color.Basic n -> "58;5;" ^ string_of_int (in_basic_range n)
  | Color.Indexed n -> "58;5;" ^ string_of_int (in_256_range n)
  | Color.Rgb (r, g, b) ->
      "58;2;"
      ^ string_of_int (in_byte_range r)
      ^ ";"
      ^ string_of_int (in_byte_range g)
      ^ ";"
      ^ string_of_int (in_byte_range b)

let underline_param = function
  | No_underline -> "24"
  | Single -> "4"
  | Double -> "4:2"
  | Curly -> "4:3"
  | Dotted -> "4:4"
  | Dashed -> "4:5"

let add_param buf p =
  if Buffer.length buf > 0 then Buffer.add_char buf ';';
  Buffer.add_string buf p

let to_sgr t =
  let buf = Buffer.create 32 in
  if t.bold then add_param buf "1";
  if t.faint then add_param buf "2";
  if t.italic then add_param buf "3";
  if t.blink then add_param buf "5";
  if t.reverse then add_param buf "7";
  if t.conceal then add_param buf "8";
  if t.strike then add_param buf "9";
  (match t.underline with No_underline -> () | u -> add_param buf (underline_param u));
  (match t.fg with Color.Default -> () | c -> add_param buf (fg_param c));
  (match t.bg with Color.Default -> () | c -> add_param buf (bg_param c));
  (match t.underline_color with
  | Color.Default -> ()
  | c -> add_param buf (underline_color_param c));
  if Buffer.length buf = 0 then "\x1b[m" else "\x1b[" ^ Buffer.contents buf ^ "m"

let transition ~from to_ =
  if equal from to_ then ""
  else if is_zero to_ then "\x1b[m"
  else begin
    let buf = Buffer.create 32 in
    if not (Color.equal from.fg to_.fg) then add_param buf (fg_param to_.fg);
    if not (Color.equal from.bg to_.bg) then add_param buf (bg_param to_.bg);
    if not (Color.equal from.underline_color to_.underline_color) then
      add_param buf (underline_color_param to_.underline_color);
    let bold_changed = from.bold <> to_.bold in
    let faint_changed = from.faint <> to_.faint in
    let bold_changed, faint_changed =
      if
        (bold_changed || faint_changed)
        && ((from.bold && not to_.bold) || (from.faint && not to_.faint))
      then begin
        add_param buf "22";
        (true, true)
      end
      else (bold_changed, faint_changed)
    in
    let italic_changed = from.italic <> to_.italic in
    if italic_changed && not to_.italic then add_param buf "23";
    let underline_changed = from.underline <> to_.underline in
    if underline_changed && to_.underline = No_underline then add_param buf "24";
    let blink_changed = from.blink <> to_.blink in
    if blink_changed && not to_.blink then add_param buf "25";
    let reverse_changed = from.reverse <> to_.reverse in
    if reverse_changed && not to_.reverse then add_param buf "27";
    let conceal_changed = from.conceal <> to_.conceal in
    if conceal_changed && not to_.conceal then add_param buf "28";
    let strike_changed = from.strike <> to_.strike in
    if strike_changed && not to_.strike then add_param buf "29";
    if bold_changed && to_.bold then add_param buf "1";
    if faint_changed && to_.faint then add_param buf "2";
    if italic_changed && to_.italic then add_param buf "3";
    if underline_changed && to_.underline = Single then add_param buf "4";
    if blink_changed && to_.blink then add_param buf "5";
    if reverse_changed && to_.reverse then add_param buf "7";
    if conceal_changed && to_.conceal then add_param buf "8";
    if strike_changed && to_.strike then add_param buf "9";
    (if underline_changed then
       match to_.underline with
       | Double | Curly | Dotted | Dashed -> add_param buf (underline_param to_.underline)
       | No_underline | Single -> ());
    "\x1b[" ^ Buffer.contents buf ^ "m"
  end

let color_value = function Some n -> n | None -> 0

let underline_of = function
  | [] | [ None ] | [ Some 1 ] -> Single
  | [ Some 0 ] -> No_underline
  | [ Some 2 ] -> Double
  | [ Some 3 ] -> Curly
  | [ Some 4 ] -> Dotted
  | [ Some 5 ] -> Dashed
  | _ -> Single

let extended_color subs rest =
  let values, rest =
    if subs <> [] then (subs, rest)
    else
      match rest with
      | [ Some 5 ] :: index :: more -> ([ Some 5 ] @ index, more)
      | [ Some 2 ] :: r :: g :: b :: more -> ([ Some 2 ] @ r @ g @ b, more)
      | _ -> ([], rest)
  in
  let color =
    match values with
    | Some 5 :: index :: _ -> (
        match Color.indexed (color_value index) with
        | Some color -> color
        | None -> Color.Default)
    | Some 2 :: _colorspace :: r :: g :: b :: _ -> (
        match Color.rgb (color_value r) (color_value g) (color_value b) with
        | Some color -> color
        | None -> Color.Default)
    | Some 2 :: r :: g :: b :: _ -> (
        match Color.rgb (color_value r) (color_value g) (color_value b) with
        | Some color -> color
        | None -> Color.Default)
    | _ -> Color.Default
  in
  (color, rest)

let rec of_sgr ~params t =
  match params with
  | [] -> t
  | parameter :: rest -> (
      match parameter with
      | [] | [ None ] | [ Some 0 ] -> of_sgr ~params:rest default
      | [ Some 1 ] -> of_sgr ~params:rest { t with bold = true }
      | [ Some 2 ] -> of_sgr ~params:rest { t with faint = true }
      | [ Some 3 ] -> of_sgr ~params:rest { t with italic = true }
      | Some 4 :: subparameters ->
          of_sgr ~params:rest { t with underline = underline_of subparameters }
      | [ Some 5 ] | [ Some 6 ] -> of_sgr ~params:rest { t with blink = true }
      | [ Some 7 ] -> of_sgr ~params:rest { t with reverse = true }
      | [ Some 8 ] -> of_sgr ~params:rest { t with conceal = true }
      | [ Some 9 ] -> of_sgr ~params:rest { t with strike = true }
      | [ Some 22 ] -> of_sgr ~params:rest { t with bold = false; faint = false }
      | [ Some 23 ] -> of_sgr ~params:rest { t with italic = false }
      | [ Some 24 ] -> of_sgr ~params:rest { t with underline = No_underline }
      | [ Some 25 ] -> of_sgr ~params:rest { t with blink = false }
      | [ Some 27 ] -> of_sgr ~params:rest { t with reverse = false }
      | [ Some 28 ] -> of_sgr ~params:rest { t with conceal = false }
      | [ Some 29 ] -> of_sgr ~params:rest { t with strike = false }
      | Some 38 :: subparameters ->
          let color, rest = extended_color subparameters rest in
          of_sgr ~params:rest { t with fg = color }
      | [ Some 39 ] -> of_sgr ~params:rest { t with fg = Color.Default }
      | Some 48 :: subparameters ->
          let color, rest = extended_color subparameters rest in
          of_sgr ~params:rest { t with bg = color }
      | [ Some 49 ] -> of_sgr ~params:rest { t with bg = Color.Default }
      | Some 58 :: subparameters ->
          let color, rest = extended_color subparameters rest in
          of_sgr ~params:rest { t with underline_color = color }
      | [ Some 59 ] -> of_sgr ~params:rest { t with underline_color = Color.Default }
      | [ Some n ] when n >= 30 && n <= 37 ->
          of_sgr ~params:rest { t with fg = Color.Basic (n - 30) }
      | [ Some n ] when n >= 40 && n <= 47 ->
          of_sgr ~params:rest { t with bg = Color.Basic (n - 40) }
      | [ Some n ] when n >= 90 && n <= 97 ->
          of_sgr ~params:rest { t with fg = Color.Basic (n - 90 + 8) }
      | [ Some n ] when n >= 100 && n <= 107 ->
          of_sgr ~params:rest { t with bg = Color.Basic (n - 100 + 8) }
      | _ -> of_sgr ~params:rest t)
