type t = { id : string; content : string; x : int; y : int; z : int; children : t list }

let v ?(id = "") ?(x = 0) ?(y = 0) ?(z = 0) content =
  { id; content; x; y; z; children = [] }

let of_content content = v content
let add t children = { t with children = t.children @ children }
let children t = t.children
let content t = t.content
let id t = t.id
let x t = t.x
let y t = t.y
let z t = t.z

let rec find t key =
  if String.equal key "" then None
  else if String.equal t.id key then Some t
  else
    Stdlib.List.fold_left
      (fun found child -> match found with Some _ -> found | None -> find child key)
      None t.children

let rec max_z t =
  Stdlib.List.fold_left (fun best child -> max best (max_z child)) t.z t.children
