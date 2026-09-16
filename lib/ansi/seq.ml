let check_text ~what s =
  let len = String.length s in
  let rec loop i =
    if i >= len then ()
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then invalid_arg (what ^ " must be valid UTF-8")
      else
        let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
        if c = 0x07 || c = 0x1b || (c >= 0x80 && c <= 0x9f) then
          invalid_arg (what ^ " contains a terminal control")
        else loop (i + Uchar.utf_decode_length d)
  in
  loop 0

let default_digits n = if n > 1 then string_of_int n else ""

let cup ~row ~col =
  if row <= 1 && col <= 1 then "\x1b[H"
  else
    let r = if row > 0 then string_of_int row else "" in
    let c = if col > 0 then string_of_int col else "" in
    "\x1b[" ^ r ^ ";" ^ c ^ "H"

let cuu n = "\x1b[" ^ default_digits n ^ "A"
let cud n = "\x1b[" ^ default_digits n ^ "B"
let cuf n = "\x1b[" ^ default_digits n ^ "C"
let cub n = "\x1b[" ^ default_digits n ^ "D"
let save_cursor = "\x1b7"
let restore_cursor = "\x1b8"

let el operand =
  match operand with `To_end -> "\x1b[K" | `To_start -> "\x1b[1K" | `All -> "\x1b[2K"

let ed operand =
  match operand with
  | `Below -> "\x1b[J"
  | `Above -> "\x1b[1J"
  | `All -> "\x1b[2J"
  | `Scrollback -> "\x1b[3J"

let decset mode = "\x1b[?" ^ string_of_int mode ^ "h"
let decrst mode = "\x1b[?" ^ string_of_int mode ^ "l"
let alt_screen = 1049
let cursor_visible = 25
let mouse_click = 1000
let mouse_motion = 1002
let mouse_all = 1003
let mouse_sgr = 1006
let mouse_pixels = 1016
let bracketed_paste = 2004
let focus = 1004
let sync_output = 2026
let grapheme_clustering = 2027

let mouse_on ~mode =
  match mode with
  | `Click -> "\x1b[?1000h\x1b[?1006h"
  | `Motion -> "\x1b[?1002h\x1b[?1006h"
  | `All -> "\x1b[?1003h\x1b[?1006h"

let mouse_off = "\x1b[?1000l\x1b[?1002l\x1b[?1003l\x1b[?1016l\x1b[?1006l"

let title s =
  check_text ~what:"title" s;
  "\x1b]2;" ^ s ^ "\x07"

let clipboard_osc52 data =
  let encoded = Base64.encode_string data in
  "\x1b]52;c;" ^ encoded ^ "\x07"

let notify_osc9 message =
  check_text ~what:"notification" message;
  "\x1b]9;" ^ message ^ "\x07"

let bg_query = "\x1b]11;?\x07"
let fg_query = "\x1b]10;?\x07"
let cursor_color_query = "\x1b]12;?\x07"
let da1 = "\x1b[c"
let xtversion = "\x1b[>q"
let kitty_push flags = "\x1b[>" ^ (if flags > 0 then string_of_int flags else "") ^ "u"
let kitty_pop = "\x1b[<u"
