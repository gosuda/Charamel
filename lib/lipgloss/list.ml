type enumerator = [ `Bullet | `Dash | `Asterisk | `Arabic | `Alphabet | `Roman ]
type t = { enumerator : enumerator; items : string list }

let v ?(enumerator = `Bullet) items = { enumerator; items }

let alphabet n =
  let rec loop n acc =
    let digit = Char.escaped (Char.chr (Char.code 'A' + (n mod 26))) in
    let next = (n / 26) - 1 in
    if next < 0 then digit ^ acc else loop next (digit ^ acc)
  in
  loop n ""

let roman n =
  let values =
    [
      (1000, "M");
      (900, "CM");
      (500, "D");
      (400, "CD");
      (100, "C");
      (90, "XC");
      (50, "L");
      (40, "XL");
      (10, "X");
      (9, "IX");
      (5, "V");
      (4, "IV");
      (1, "I");
    ]
  in
  let rec loop n = function
    | [] -> ""
    | (value, glyph) :: rest ->
        if n >= value then glyph ^ loop (n - value) ((value, glyph) :: rest)
        else loop n rest
  in
  loop n values

let marker kind index ~roman_width =
  match kind with
  | `Bullet -> "• "
  | `Dash -> "- "
  | `Asterisk -> "* "
  | `Arabic -> string_of_int (index + 1) ^ ". "
  | `Alphabet -> alphabet index ^ ". "
  | `Roman ->
      let value = roman (index + 1) in
      String.make (max 0 (roman_width - String.length value)) ' ' ^ value ^ ". "

let render t =
  let roman_width =
    match t.enumerator with
    | `Roman ->
        Stdlib.List.fold_left
          (fun width index -> max width (String.length (roman (index + 1))))
          0
          (Stdlib.List.init (Stdlib.List.length t.items) Fun.id)
    | _ -> 0
  in
  let out = Buffer.create 128 in
  Stdlib.List.iteri
    (fun index item ->
      if index > 0 then Buffer.add_char out '\n';
      let prefix = marker t.enumerator index ~roman_width in
      let prefix_width = Charamel_ansi.Text.width prefix in
      match String.split_on_char '\n' item with
      | [] -> Buffer.add_string out prefix
      | first :: rest ->
          Buffer.add_string out prefix;
          Buffer.add_string out first;
          Stdlib.List.iter
            (fun line ->
              Buffer.add_char out '\n';
              Buffer.add_string out (String.make prefix_width ' ');
              Buffer.add_string out line)
            rest)
    t.items;
  Buffer.contents out
