let win32_metacharacters = [ '<'; '>'; '|'; '('; ')'; '&'; '^'; '!'; '"'; ' '; '\t' ]

let escape_win32 text =
  let buffer = Buffer.create (String.length text + 8) in
  String.iter
    (fun character ->
      if List.mem character win32_metacharacters then Buffer.add_char buffer '^';
      Buffer.add_char buffer character)
    text;
  Buffer.contents buffer

let quote_posix text = "'" ^ String.concat "'\\''" (String.split_on_char '\'' text) ^ "'"
let quote text = if Sys.win32 then "\"" ^ escape_win32 text ^ "\"" else quote_posix text

let with_directory cwd text =
  match cwd with
  | None -> text
  | Some directory ->
      if Sys.win32 then "cd /d " ^ quote directory ^ " && " ^ text
      else "cd " ^ quote directory ^ " && " ^ text

let command ?cwd text =
  if Sys.win32 then [ "cmd.exe"; "/d"; "/s"; "/c"; with_directory cwd text ]
  else [ "/bin/sh"; "-c"; with_directory cwd text ]

let is_space = function ' ' | '\t' -> true | _ -> false

(* One pass over the text: outside a quote, whitespace closes the current word; inside one,
   every character including whitespace belongs to the word. The delimiter that opened a quote
   is the only one that closes it, so a double-quoted word can contain an apostrophe. *)
let split_words text =
  let length = String.length text in
  let rec word index quote_character current words =
    if index = length then
      if Buffer.length current = 0 then List.rev words
      else List.rev (Buffer.contents current :: words)
    else
      let character = text.[index] in
      match quote_character with
      | Some delimiter when Char.equal character delimiter ->
          word (index + 1) None current words
      | Some delimiter ->
          Buffer.add_char current character;
          word (index + 1) (Some delimiter) current words
      | None when Char.equal character '\'' || Char.equal character '"' ->
          word (index + 1) (Some character) current words
      | None when is_space character ->
          if Buffer.length current = 0 then word (index + 1) None current words
          else word (index + 1) None (Buffer.create 16) (Buffer.contents current :: words)
      | None ->
          Buffer.add_char current character;
          word (index + 1) None current words
  in
  word 0 None (Buffer.create 32) []
