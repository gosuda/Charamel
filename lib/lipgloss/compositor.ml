type bounds = { x0 : int; y0 : int; x1 : int; y1 : int }
type hit = { id : string; layer : Layer.t; bounds : bounds }
type flat = { layer : Layer.t; bounds : bounds }
type t = { root : Layer.t; flat : flat list; bounds : bounds option }

let rect layer ~abs_x ~abs_y =
  let content = Layer.content layer in
  let width = Layout.width content and height = Layout.height content in
  { x0 = abs_x; y0 = abs_y; x1 = abs_x + width; y1 = abs_y + height }

let rec flatten layer ~abs_x ~abs_y acc =
  let x = abs_x + Layer.x layer and y = abs_y + Layer.y layer in
  let entry = { layer; bounds = rect layer ~abs_x:x ~abs_y:y } in
  Stdlib.List.fold_left
    (fun acc child -> flatten child ~abs_x:x ~abs_y:y acc)
    (entry :: acc) (Layer.children layer)

let union a b =
  { x0 = min a.x0 b.x0; y0 = min a.y0 b.y0; x1 = max a.x1 b.x1; y1 = max a.y1 b.y1 }

let covers bounds ~x ~y =
  x >= bounds.x0 && x < bounds.x1 && y >= bounds.y0 && y < bounds.y1

let by_z (a : flat) (b : flat) = compare (Layer.z a.layer) (Layer.z b.layer)

let build root =
  let flat = Stdlib.List.rev (flatten root ~abs_x:0 ~abs_y:0 []) in
  let ordered = Stdlib.List.stable_sort by_z flat in
  let bounds =
    match ordered with
    | [] -> None
    | { bounds = first; _ } :: rest ->
        let merge acc ({ bounds; _ } : flat) = union acc bounds in
        Some (Stdlib.List.fold_left merge first rest)
  in
  { root; flat = ordered; bounds }

let v layers = build (Layer.add (Layer.of_content "") layers)
let add t layers = build (Layer.add t.root layers)
let bounds t = t.bounds
let find t id = Layer.find t.root id
let refresh t = t

let rec top_entry entries ~x ~y =
  match entries with
  | [] -> None
  | ({ layer; bounds } : flat) :: rest ->
      let key = Layer.id layer in
      if key <> "" && covers bounds ~x ~y then Some { id = key; layer; bounds }
      else top_entry rest ~x ~y

let hit t ~x ~y = top_entry (Stdlib.List.rev t.flat) ~x ~y

let draw_one ~x0 ~y0 canvas (entry : flat) =
  Canvas.draw canvas ~x:(entry.bounds.x0 - x0) ~y:(entry.bounds.y0 - y0)
    (Layer.content entry.layer)

let render t =
  match t.bounds with
  | None -> ""
  | Some { x0; y0; x1; y1 } when x1 - x0 <= 0 || y1 - y0 <= 0 -> ""
  | Some { x0; y0; x1; y1 } ->
      let canvas = Canvas.create ~width:(x1 - x0) ~height:(y1 - y0) in
      Canvas.render (Stdlib.List.fold_left (draw_one ~x0 ~y0) canvas t.flat)
