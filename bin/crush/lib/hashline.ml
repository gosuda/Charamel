let prime1 = 0x9E3779B1l
let prime2 = 0x85EBCA77l
let prime3 = 0xC2B2AE3Dl
let prime4 = 0x27D4EB2Fl
let prime5 = 0x165667B1l

let rotate_left value bits =
  Int32.logor (Int32.shift_left value bits) (Int32.shift_right_logical value (32 - bits))

let read32 data offset =
  let byte index = Int32.of_int (String.get_uint8 data index) in
  Int32.logor (byte offset)
    (Int32.logor
       (Int32.shift_left (byte (offset + 1)) 8)
       (Int32.logor
          (Int32.shift_left (byte (offset + 2)) 16)
          (Int32.shift_left (byte (offset + 3)) 24)))

let round accumulator input =
  Int32.mul (rotate_left (Int32.add accumulator (Int32.mul input prime2)) 13) prime1

let avalanche value =
  let value = Int32.logxor value (Int32.shift_right_logical value 15) in
  let value = Int32.mul value prime2 in
  let value = Int32.logxor value (Int32.shift_right_logical value 13) in
  let value = Int32.mul value prime3 in
  Int32.logxor value (Int32.shift_right_logical value 16)

let xxh32 data seed =
  let length = String.length data in
  let initial, offset =
    if length >= 16 then
      let limit = length - 16 in
      let rec consume offset v1 v2 v3 v4 =
        if offset <= limit then
          consume (offset + 16)
            (round v1 (read32 data offset))
            (round v2 (read32 data (offset + 4)))
            (round v3 (read32 data (offset + 8)))
            (round v4 (read32 data (offset + 12)))
        else
          let mixed =
            Int32.add
              (Int32.add (rotate_left v1 1) (rotate_left v2 7))
              (Int32.add (rotate_left v3 12) (rotate_left v4 18))
          in
          (mixed, offset)
      in
      consume 0
        (Int32.add (Int32.add seed prime1) prime2)
        (Int32.add seed prime2) seed (Int32.sub seed prime1)
    else (Int32.add seed prime5, 0)
  in
  let value = Int32.add initial (Int32.of_int length) in
  let value, offset =
    let limit = length - 4 in
    let rec consume offset value =
      if offset <= limit then
        let value =
          Int32.mul
            (rotate_left (Int32.add value (Int32.mul (read32 data offset) prime3)) 17)
            prime4
        in
        consume (offset + 4) value
      else (value, offset)
    in
    consume offset value
  in
  let rec consume_tail offset value =
    if offset < length then
      let value =
        Int32.mul
          (rotate_left
             (Int32.add value
                (Int32.mul (Int32.of_int (String.get_uint8 data offset)) prime5))
             11)
          prime1
      in
      consume_tail (offset + 1) value
    else value
  in
  avalanche (consume_tail offset value)

let normalize s =
  let length = String.length s in
  let result = Buffer.create length in
  let rec strip_end start stop =
    if stop <= start then start
    else
      match String.get s (stop - 1) with
      | ' ' | '\t' | '\r' -> strip_end start (stop - 1)
      | _ -> stop
  in
  let rec add_lines start =
    if start < length then begin
      let newline =
        match String.index_from_opt s start '\n' with
        | Some position -> position
        | None -> length
      in
      let stop = strip_end start newline in
      Buffer.add_substring result s start (stop - start);
      if newline < length then begin
        Buffer.add_char result '\n';
        add_lines (newline + 1)
      end
    end
  in
  add_lines 0;
  Buffer.contents result

let tag s =
  let low = Int32.logand (xxh32 (normalize s) 0l) 0xFFFFl in
  Fmt.str "%04X" (Int32.to_int low)
