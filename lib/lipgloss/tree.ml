type enumerator = depth:int -> index:int -> last:bool -> string
type indenter = depth:int -> index:int -> last:bool -> string
type style_func = depth:int -> index:int -> value:string -> Style.t

type t_style = {
  root : Style.t;
  item : style_func;
  enumerator : style_func;
  indenter : style_func;
}

type t = {
  value : string;
  hidden : bool;
  offset : (int * int) option;
  children : t list;
  styles : t_style option;
  width : int option;
  enumerator : [ `Default | `Rounded | `Custom of enumerator ] option;
  indenter : [ `Default | `Custom of indenter ] option;
}

type renderer = {
  enumerator : enumerator;
  indenter : indenter;
  styles : t_style;
  width : int option;
}

let default_enumerator ~depth:_ ~index:_ ~last = if last then "└── " else "├── "

let rounded_enumerator ~depth:_ ~index:_ ~last = if last then "╰── " else "├── "

let default_indenter ~depth ~index:_ ~last =
  if depth = 0 then "" else if last then "    " else "│   "

let plain_styles =
  {
    root = Style.empty;
    item = (fun ~depth:_ ~index:_ ~value:_ -> Style.empty);
    enumerator = (fun ~depth:_ ~index:_ ~value:_ -> Style.empty);
    indenter = (fun ~depth:_ ~index:_ ~value:_ -> Style.empty);
  }

let base_renderer =
  {
    enumerator = default_enumerator;
    indenter = default_indenter;
    styles = plain_styles;
    width = None;
  }

let empty =
  {
    value = "";
    hidden = false;
    offset = None;
    children = [];
    styles = None;
    width = None;
    enumerator = None;
    indenter = None;
  }

let leaf ?(hidden = false) value = { empty with value; hidden }

let node ?(value = "") ?(hidden = false) ?offset ?(children = []) () =
  { empty with value; hidden; offset; children }

let root value (t : t) = { t with value }
let style (t : t) styles = { t with styles = Some styles }
let width width (t : t) = { t with width = Some width }
let hidden t = t.hidden
let hide hidden (t : t) = { t with hidden }
let children t = t.children
let value t = t.value
let enumerate enumerator (t : t) = { t with enumerator = Some enumerator }
let indent indenter (t : t) = { t with indenter = Some indenter }

let enumerator_of_kind = function
  | `Default -> default_enumerator
  | `Rounded -> rounded_enumerator
  | `Custom f -> f

let indenter_of_kind = function `Default -> default_indenter | `Custom f -> f

let own_renderer (t : t) =
  match (t.styles, t.enumerator, t.indenter, t.width) with
  | None, None, None, None -> None
  | styles, enumerator, indenter, width ->
      Some
        {
          enumerator =
            (match enumerator with
            | Some k -> enumerator_of_kind k
            | None -> default_enumerator);
          indenter =
            (match indenter with
            | Some k -> indenter_of_kind k
            | None -> default_indenter);
          styles = (match styles with Some s -> s | None -> plain_styles);
          width;
        }

let resolve parent t = match own_renderer t with Some r -> r | None -> parent
let clamp n v = max 0 (min n v)

let visible (t : t) =
  let kids =
    match t.offset with
    | None -> t.children
    | Some (start, stop) ->
        let count = Stdlib.List.length t.children in
        let start, stop = if start > stop then (stop, start) else (start, stop) in
        let lo = clamp count start and hi = clamp count stop in
        Stdlib.List.filteri (fun i _ -> i >= lo && i < hi) t.children
  in
  Stdlib.List.filter (fun child -> not child.hidden) kids

let line_width text = Layout.width text

let nth_default values index =
  match Stdlib.List.nth_opt values index with Some v -> v | None -> ""

let pad_line width style line =
  let pad = width - line_width line in
  if pad <= 0 then line else line ^ Style.render style (String.make pad ' ')

type block = {
  marker_width : int;
  value_style : Style.t;
  own : string list;
  tail : string list;
}

let render ?enumerator ?indenter t =
  let start =
    let base = match own_renderer t with Some r -> r | None -> base_renderer in
    {
      base with
      enumerator = (match enumerator with Some e -> e | None -> base.enumerator);
      indenter = (match indenter with Some i -> i | None -> base.indenter);
    }
  in
  let pad_marker width style line =
    match width with Some w -> pad_line w style line | None -> line
  in
  let rec walk r depth index prefix last branch node =
    let kids = visible node in
    let last_index = Stdlib.List.length kids - 1 in
    let value_style =
      if depth = 0 then r.styles.root else r.styles.item ~depth ~index ~value:node.value
    in
    let group = String.equal node.value "" && kids <> [] in
    let values =
      if group then []
      else String.split_on_char '\n' (Style.render value_style node.value)
    in
    let branch_lines = if depth = 0 then [ "" ] else String.split_on_char '\n' branch in
    let branch_width =
      Stdlib.List.fold_left (fun w line -> max w (line_width line)) 0 branch_lines
    in
    let own =
      if group then []
      else
        let line_count =
          max (Stdlib.List.length branch_lines) (Stdlib.List.length values)
        in
        Stdlib.List.init line_count (fun line_index ->
            let branch_line = nth_default branch_lines line_index in
            let gap = String.make (max 0 (branch_width - line_width branch_line)) ' ' in
            let value = nth_default values line_index in
            if depth = 0 then value else branch_line ^ gap ^ value)
    in
    let child_blocks =
      Stdlib.List.mapi
        (fun child_index child ->
          let child_last = child_index = last_index in
          let indent_glyph =
            if depth = 0 then ""
            else
              Style.render
                (r.styles.indenter ~depth ~index ~value:child.value)
                (r.indenter ~depth ~index ~last)
          in
          let child_prefix = prefix ^ indent_glyph in
          let branch_glyph =
            Style.render
              (r.styles.enumerator ~depth:(depth + 1) ~index:child_index
                 ~value:child.value)
              (r.enumerator ~depth:(depth + 1) ~index:child_index ~last:child_last)
          in
          let child_r = resolve r child in
          let block =
            walk child_r (depth + 1) child_index child_prefix child_last branch_glyph
              child
          in
          (child_r, child_prefix, block))
        kids
    in
    let widest =
      Stdlib.List.fold_left
        (fun w (_, _, block) -> max w block.marker_width)
        0 child_blocks
    in
    let tail =
      Stdlib.List.concat_map
        (fun (child_r, child_prefix, block) ->
          let spaces = String.make (max 0 (widest - block.marker_width)) ' ' in
          let own =
            Stdlib.List.map
              (fun line ->
                pad_marker child_r.width block.value_style @@ child_prefix ^ spaces ^ line)
              block.own
          in
          own @ block.tail)
        child_blocks
    in
    { marker_width = branch_width; value_style; own; tail }
  in
  if t.hidden then ""
  else
    let block = walk start 0 0 "" true "" t in
    let own = Stdlib.List.map (pad_marker start.width block.value_style) block.own in
    String.concat "\n" (own @ block.tail)
