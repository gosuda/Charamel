type status = Pending | In_progress | Completed
type item = { content : string; status : status; active_form : string }
type t = { mutable items : item list }
type transition_error = [ `Invalid_index of int | `Invalid_transition of status * status ]

let create () = { items = [] }
let set t items = t.items <- List.map (fun item -> item) items
let get t = List.map (fun item -> item) t.items

let valid_transition from_status to_status =
  match (from_status, to_status) with
  | Pending, Pending | In_progress, In_progress | Completed, Completed -> true
  | Pending, In_progress | In_progress, Completed -> true
  | _ -> false

let replace_at index value items =
  let rec loop current = function
    | [] -> []
    | item :: rest ->
        if current = index then value :: rest else item :: loop (current + 1) rest
  in
  loop 0 items

let transition t ~index status =
  if index < 0 then Error (`Invalid_index index)
  else
    match List.nth_opt t.items index with
    | None -> Error (`Invalid_index index)
    | Some item when not (valid_transition item.status status) ->
        Error (`Invalid_transition (item.status, status))
    | Some item ->
        t.items <- replace_at index { item with status } t.items;
        Ok ()

let status_jsont =
  Jsont.enum
    [ ("pending", Pending); ("in_progress", In_progress); ("completed", Completed) ]

let item_jsont =
  let open Jsont in
  Object.map (fun content status active_form -> { content; status; active_form })
  |> Object.mem "content" string ~enc:(fun item -> item.content)
  |> Object.mem "status" status_jsont ~enc:(fun item -> item.status)
  |> Object.mem "active_form" string ~enc:(fun item -> item.active_form)
  |> Object.finish

let jsont = Jsont.list item_jsont

let render items =
  items
  |> List.map (fun item ->
      match item.status with
      | Pending -> "[ ] " ^ item.content
      | In_progress -> "[>] " ^ item.active_form
      | Completed -> "[x] " ^ item.content)
  |> String.concat "\n"
