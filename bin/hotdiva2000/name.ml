let mask_for ~bound =
  if bound < 1 then Fmt.invalid_arg "Name: bound must be >= 1";
  let rec go m = if m >= bound - 1 then m else go ((m lsl 1) lor 1) in
  go 0

let bytes_needed ~bound =
  let mask = mask_for ~bound in
  let rec count m acc = if m = 0 then acc else count (m lsr 8) (acc + 1) in
  max 1 (count mask 0)

let index_of_bytes ~bound bytes =
  let mask = mask_for ~bound in
  let expected = bytes_needed ~bound in
  if String.length bytes <> expected then
    Fmt.invalid_arg "Name.index_of_bytes: expected %d byte(s), got %d" expected
      (String.length bytes);
  let raw = String.fold_left (fun acc c -> (acc lsl 8) lor Char.code c) 0 bytes in
  let candidate = raw land mask in
  if candidate < bound then Some candidate else None

(* Draw a uniformly-distributed index in [0, bound) from [random]: compute
   the smallest all-ones bitmask covering [bound], draw that many bytes,
   mask, and reject draws landing outside [bound] so every index is equally
   likely. A raw draw is never reduced with [mod]: that would bias every
   index below [(1 lsl (8 * bytes_needed)) mod bound] to appear slightly
   more often. *)
let random_index ~random ~bound =
  let n = bytes_needed ~bound in
  let rec draw () =
    match index_of_bytes ~bound (random n) with Some i -> i | None -> draw ()
  in
  draw ()

let pick ~random words = words.(random_index ~random ~bound:(Array.length words))

let generate ~random ~separator ~tokens () =
  if tokens < 1 then Fmt.invalid_arg "Name.generate: tokens must be >= 1";
  let words =
    List.init tokens (fun i ->
        if i = tokens - 1 then pick ~random Words.nouns else pick ~random Words.modifiers)
  in
  String.concat " " words |> String.lowercase_ascii |> String.split_on_char ' '
  |> String.concat separator

let generate_many ~random ~count ~separator ~tokens () =
  if count < 0 then Fmt.invalid_arg "Name.generate_many: count must be >= 0";
  if tokens < 1 then Fmt.invalid_arg "Name.generate_many: tokens must be >= 1";
  List.init count (fun _ -> generate ~random ~separator ~tokens ())
