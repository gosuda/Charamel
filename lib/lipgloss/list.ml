type marker =
  [ `Bullet
  | `Dash
  | `Asterisk
  | `Arabic
  | `Alphabet
  | `Roman
  | `Custom of index:int -> string ]

type item = Text of string | Nested of t

and t = {
  marker : marker;
  indent : index:int -> string;
  item_style : index:int -> Style.t;
  marker_style : index:int -> Style.t;
  indent_style : index:int -> Style.t;
  items : item list;
  hidden : bool;
  offset : (int * int) option;
}

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

let marker_text marker index =
  match marker with
  | `Bullet -> "• "
  | `Dash -> "- "
  | `Asterisk -> "* "
  | `Arabic -> string_of_int (index + 1) ^ ". "
  | `Alphabet -> alphabet index ^ ". "
  | `Roman -> roman (index + 1) ^ ". "
  | `Custom f -> f ~index ^ " "

let marker_of_string name =
  match name with
  | "bullet" -> Some `Bullet
  | "dash" -> Some `Dash
  | "asterisk" -> Some `Asterisk
  | "arabic" -> Some `Arabic
  | "alphabet" -> Some `Alphabet
  | "roman" -> Some `Roman
  | _ -> None

let plain = fun ~index:_ -> Style.empty
let gap = fun ~index:_ -> "  "

let v ?(marker = `Bullet) ?(indent = gap) ?(item_style = plain) ?(marker_style = plain)
    ?(indent_style = plain) items =
  {
    marker;
    indent;
    item_style;
    marker_style;
    indent_style;
    items;
    hidden = false;
    offset = None;
  }

let item new_item t = { t with items = t.items @ [ new_item ] }
let offset start stop t = { t with offset = Some (start, stop) }
let hide hidden t = { t with hidden }

let rec to_tree t =
  let children =
    Stdlib.List.map
      (function Text text -> Tree.leaf text | Nested sub -> to_tree sub)
      t.items
  in
  let styles =
    {
      Tree.root = Style.empty;
      item = (fun ~depth:_ ~index ~value:_ -> t.item_style ~index);
      enumerator = (fun ~depth:_ ~index ~value:_ -> t.marker_style ~index);
      indenter = (fun ~depth:_ ~index ~value:_ -> t.indent_style ~index);
    }
  in
  let node = Tree.node ~hidden:t.hidden ?offset:t.offset ~children () in
  let node = Tree.style node styles in
  let node =
    Tree.enumerate
      (`Custom (fun ~depth:_ ~index ~last:_ -> marker_text t.marker index))
      node
  in
  Tree.indent (`Custom (fun ~depth:_ ~index ~last:_ -> t.indent ~index)) node

let render t = Tree.render (to_tree t)
