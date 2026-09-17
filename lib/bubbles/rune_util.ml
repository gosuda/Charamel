let clamp n lo hi = max lo (min hi n)
let cluster_width = Charamel_ansi.Width.grapheme_width

let take n xs =
  let rec loop remaining acc = function
    | [] -> Stdlib.List.rev acc
    | _ when remaining <= 0 -> Stdlib.List.rev acc
    | x :: rest -> loop (remaining - 1) (x :: acc) rest
  in
  loop n [] xs

let drop n xs =
  let rec loop remaining = function
    | [] -> []
    | xs when remaining <= 0 -> xs
    | _ :: rest -> loop (remaining - 1) rest
  in
  loop n xs

let map_case mapping s =
  let out = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then ()
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then loop (i + 1)
      else
        let u = Uchar.utf_decode_uchar d in
        let n = Uchar.utf_decode_length d in
        (match mapping u with
        | `Self -> Buffer.add_utf_8_uchar out u
        | `Uchars us -> Stdlib.List.iter (Buffer.add_utf_8_uchar out) us);
        loop (i + n)
  in
  loop 0;
  Buffer.contents out

let lower s = map_case Uucp.Case.Map.to_lower s
let upper s = map_case Uucp.Case.Map.to_upper s

let is_control u =
  let n = Uchar.to_int u in
  (n >= 0 && n <= 0x1f) || (n >= 0x7f && n <= 0x9f)

let sanitize ~replace_tabs ~replace_newlines ~normalize_crlf s =
  let length = String.length s in
  let out = Buffer.create length in
  let rec loop i =
    if i >= length then ()
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then loop (i + 1)
      else
        let u = Uchar.utf_decode_uchar d in
        let n = Uchar.utf_decode_length d in
        if Uchar.equal u (Uchar.of_char '\t') then Buffer.add_string out replace_tabs
        else if Uchar.equal u (Uchar.of_char '\n') then
          Buffer.add_string out replace_newlines
        else if Uchar.equal u (Uchar.of_char '\r') then
          if normalize_crlf && i + n < length && String.get s (i + n) = '\n' then ()
          else Buffer.add_string out replace_newlines
        else if not (is_control u) then Buffer.add_utf_8_uchar out u;
        loop (i + n)
  in
  loop 0;
  Buffer.contents out

let whitespace_cluster s =
  if s = "" then false
  else
    let d = String.get_utf_8_uchar s 0 in
    Uchar.utf_decode_is_valid d && Uucp.White.is_white_space (Uchar.utf_decode_uchar d)
