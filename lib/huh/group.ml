type t = {
  title : string;
  description : string;
  hide : (Results.t -> bool) option;
  show_help : bool;
  show_errors : bool;
  fields : Field_impl.t array;
  selected : int;
  active : bool;
  width : int;
  height : int;
  y_offset : int;
}

let v ?(title = "") ?(description = "") ?hide ?(show_help = true) ?(show_errors = true)
    fields =
  {
    title;
    description;
    hide;
    show_help;
    show_errors;
    fields = Array.of_list fields;
    selected = 0;
    active = false;
    width = 80;
    height = 0;
    y_offset = 0;
  }

let is_hidden ~results g =
  Array.length g.fields = 0
  || match g.hide with Some predicate -> predicate results | None -> false

let selected g = g.selected

let set_selected selected g =
  { g with selected = max 0 (min selected (max 0 (Array.length g.fields - 1))) }

let active g = g.active
let set_active active g = { g with active }
let width g = g.width
let height g = g.height
let set_size ~width ~height g = { g with width = max 0 width; height = max 0 height }
let y_offset g = g.y_offset
let set_y_offset offset g = { g with y_offset = max 0 offset }
let field_count g = Array.length g.fields

let field index g =
  if index < 0 || index >= Array.length g.fields then
    invalid_arg "Charamel_huh.Group.field"
  else g.fields.(index)

let set_field index value g =
  if index < 0 || index >= Array.length g.fields then
    invalid_arg "Charamel_huh.Group.set_field"
  else
    let fields = Array.copy g.fields in
    fields.(index) <- value;
    { g with fields }

let focused_field g =
  if g.selected < 0 || g.selected >= Array.length g.fields then None
  else Some g.fields.(g.selected)

let visible_indices ~skip g =
  if Array.length g.fields = 1 then [ 0 ]
  else
    let result = ref [] in
    Array.iteri
      (fun index _ -> if not (skip index) then result := index :: !result)
      g.fields;
    List.rev !result

let first_index ~skip g =
  if Array.length g.fields = 1 then Some 0
  else
    let rec loop index =
      if index >= Array.length g.fields then None
      else if skip index then loop (index + 1)
      else Some index
    in
    loop 0

let last_index ~skip g =
  if Array.length g.fields = 1 then Some 0
  else
    let found = ref None in
    Array.iteri (fun index _ -> if not (skip index) then found := Some index) g.fields;
    !found

let next_index ~skip g index =
  if Array.length g.fields = 1 then None
  else
    let rec loop candidate =
      if candidate >= Array.length g.fields then None
      else if skip candidate then loop (candidate + 1)
      else Some candidate
    in
    loop (max 0 (index + 1))

let previous_index ~skip g index =
  if Array.length g.fields = 1 then None
  else
    let rec loop candidate =
      if candidate < 0 then None
      else if skip candidate then loop (candidate - 1)
      else Some candidate
    in
    loop (min (Array.length g.fields - 1) (index - 1))

let errors g =
  let reversed =
    Array.fold_left
      (fun acc field ->
        match Field_impl.error field with Some error -> error :: acc | None -> acc)
      [] g.fields
  in
  List.rev reversed

let content_lines ~separator views = String.concat separator (List.map snd views)
