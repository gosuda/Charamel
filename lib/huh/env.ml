type t = {
  fs_root : string;
  temp_dir : string;
  editor : string list option;
  clock : Charamel_os.Time.clock;
}

let v ~fs_root ~temp_dir ~editor ~clock = { fs_root; temp_dir; editor; clock }

let editor_of_string value =
  let source =
    match value with Some text when String.trim text <> "" -> text | _ -> "nano"
  in
  let length = String.length source in
  let is_separator = function ' ' | '\t' -> true | _ -> false in
  let rec skip index =
    if index < length && is_separator source.[index] then skip (index + 1) else index
  in
  let rec token start index =
    if index = length then
      if start = index then [] else [ String.sub source start (index - start) ]
    else if is_separator source.[index] then
      let item = String.sub source start (index - start) in
      let next = skip index in
      if item = "" then token next next else item :: token next next
    else token start (index + 1)
  in
  let start = skip 0 in
  if start = length then [ "nano" ] else token start start
