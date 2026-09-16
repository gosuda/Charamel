type t = { value : string; children : t list }
type enumerator = depth:int -> index:int -> last:bool -> string
type indenter = depth:int -> last:bool -> string

type rendered = {
  own : string;
  tail : string;
  root_prefix : string;
  root_suffixes : string list;
  root_width : int;
}

let leaf value = { value; children = [] }
let node ?(value = "") children = { value; children }

let default_enumerator ~depth:_ ~index:_ ~last = if last then "└── " else "├── "

let default_indenter ~depth ~last =
  if depth = 0 then "" else if last then "    " else "│   "

let render ?(enumerator = default_enumerator) ?(indenter = default_indenter) ?style root =
  let style_value depth value =
    match style with None -> value | Some f -> Style.render (f ~depth value) value
  in
  let line_width value = Layout.width value in
  let nth_default values index =
    match Stdlib.List.nth_opt values index with Some value -> value | None -> ""
  in
  let rendered_text value =
    if value.own = "" then value.tail
    else if value.tail = "" then value.own
    else value.own ^ "\n" ^ value.tail
  in
  let pad_root width value =
    let amount = max 0 (width - value.root_width) in
    if amount = 0 || value.root_suffixes = [] then value
    else
      let spaces = String.make amount ' ' in
      let own =
        value.root_suffixes
        |> Stdlib.List.map (fun suffix -> value.root_prefix ^ spaces ^ suffix)
        |> String.concat "\n"
      in
      { value with own; root_width = width }
  in
  let rec walk depth prefix last branch_thunk node =
    let values =
      if depth = 0 && node.value = "" then []
      else String.split_on_char '\n' (style_value depth node.value)
    in
    let branch = branch_thunk () in
    let branch_lines = if depth = 0 then [ "" ] else String.split_on_char '\n' branch in
    let branch_width =
      Stdlib.List.fold_left (fun width line -> max width (line_width line)) 0 branch_lines
    in
    let root_suffixes =
      if depth = 0 && node.value = "" then []
      else
        let line_count =
          max (Stdlib.List.length branch_lines) (Stdlib.List.length values)
        in
        Stdlib.List.init line_count (fun line_index ->
            let branch_line = nth_default branch_lines line_index in
            let branch_line_width = line_width branch_line in
            let value = nth_default values line_index in
            let gap = String.make (max 0 (branch_width - branch_line_width)) ' ' in
            if depth = 0 then value else branch_line ^ gap ^ value)
    in
    let own =
      root_suffixes
      |> Stdlib.List.map (fun suffix -> prefix ^ suffix)
      |> String.concat "\n"
    in
    let last_child = Stdlib.List.length node.children - 1 in
    let child_results =
      Stdlib.List.mapi
        (fun child_index child ->
          let child_last = child_index = last_child in
          let child_prefix = prefix ^ if depth = 0 then "" else indenter ~depth ~last in
          let child_branch () =
            enumerator ~depth:(depth + 1) ~index:child_index ~last:child_last
          in
          walk (depth + 1) child_prefix child_last child_branch child)
        node.children
    in
    let child_width =
      Stdlib.List.fold_left
        (fun width child -> max width child.root_width)
        0 child_results
    in
    let tail =
      child_results
      |> Stdlib.List.map (fun child -> rendered_text (pad_root child_width child))
      |> String.concat "\n"
    in
    { own; tail; root_prefix = prefix; root_suffixes; root_width = branch_width }
  in
  rendered_text (walk 0 "" true (fun () -> "") root)
