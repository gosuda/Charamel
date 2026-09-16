type t = { url : string; params : (string * string) list }

let check_text s =
  let len = String.length s in
  let rec loop i =
    if i >= len then ()
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then invalid_arg "Link.osc8: malformed UTF-8"
      else
        let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
        if c = 0x07 || c = 0x1b || (c >= 0x80 && c <= 0x9f) then
          invalid_arg "Link.osc8: terminal control"
        else loop (i + Uchar.utf_decode_length d)
  in
  loop 0

let parameter (key, value) =
  check_text key;
  check_text value;
  if
    key = "" || String.contains key '=' || String.contains key ':'
    || String.contains key ';' || String.contains value ':' || String.contains value ';'
  then invalid_arg "Link.osc8: invalid parameter";
  key ^ "=" ^ value

let osc8 = function
  | None -> "\x1b]8;;\x07"
  | Some { url; params } ->
      if url = "" then invalid_arg "Link.osc8: empty URL";
      check_text url;
      let params = String.concat ":" (List.map parameter params) in
      "\x1b]8;" ^ params ^ ";" ^ url ^ "\x07"
