let check ~what s =
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
