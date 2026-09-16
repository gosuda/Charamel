let scalar_count text =
  let rec loop offset count =
    if offset >= String.length text then count
    else
      let decoded = String.get_utf_8_uchar text offset in
      let step =
        if Uchar.utf_decode_is_valid decoded then Uchar.utf_decode_length decoded else 1
      in
      loop (offset + max 1 step) (count + 1)
  in
  loop 0 0

let not_empty value = if value = "" then Error "input cannot be empty" else Ok ()

let min_length minimum value =
  if scalar_count value < minimum then
    Error (Fmt.str "input must be at least %d characters long" minimum)
  else Ok ()

let max_length maximum value =
  if scalar_count value > maximum then
    Error (Fmt.str "input must be at most %d characters long" maximum)
  else Ok ()

let length ~min ~max value =
  match min_length min value with
  | Ok () -> max_length max value
  | Error _ as error -> error

let one_of values value =
  if Stdlib.List.exists (String.equal value) values then Ok ()
  else Error (Fmt.str "invalid option: %s" value)

let all validators value =
  let rec loop = function
    | [] -> Ok ()
    | validator :: rest -> (
        match validator value with Ok () -> loop rest | Error _ as error -> error)
  in
  loop validators
