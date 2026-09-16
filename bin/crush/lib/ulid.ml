let alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
let alphabet_length = String.length alphabet
let max_timestamp = (1 lsl 48) - 1
let digit value = alphabet.[value land 31]
let byte_at bytes index = Char.code bytes.[index]

let v ~now_ms ~random =
  if now_ms < 0 || now_ms > max_timestamp then
    invalid_arg "Ulid.v: timestamp is outside the 48-bit range"
  else
    let entropy = random 10 in
    if String.length entropy <> 10 then
      invalid_arg "Ulid.v: random function must return exactly 10 bytes"
    else
      let result = Bytes.create 26 in
      for index = 0 to 9 do
        let shift = (9 - index) * 5 in
        Bytes.set result index (digit (now_ms lsr shift))
      done;
      for index = 0 to 15 do
        let first_bit = index * 5 in
        let value = ref 0 in
        for bit = 0 to 4 do
          let position = first_bit + bit in
          let byte = byte_at entropy (position / 8) in
          let bit_value = (byte lsr (7 - (position mod 8))) land 1 in
          value := (!value lsl 1) lor bit_value
        done;
        Bytes.set result (10 + index) (digit !value)
      done;
      Bytes.unsafe_to_string result

let decode_char c =
  let rec loop index =
    if index = alphabet_length then None
    else if alphabet.[index] = c then Some index
    else loop (index + 1)
  in
  loop 0

let timestamp_ms id =
  if String.length id <> 26 then None
  else
    let rec loop index timestamp =
      if index = 26 then Some timestamp
      else
        match decode_char id.[index] with
        | None -> None
        | Some value when index = 0 && value > 7 -> None
        | Some value ->
            let timestamp =
              if index < 10 then (timestamp lsl 5) lor value else timestamp
            in
            loop (index + 1) timestamp
    in
    loop 0 0

let is_valid id = Option.is_some (timestamp_ms id)
