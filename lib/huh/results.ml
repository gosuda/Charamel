type binding = B : 'a Key.t * 'a -> binding

module Int_map = Map.Make (Int)

type t = { bindings : binding Int_map.t; version : int }

let empty = { bindings = Int_map.empty; version = 0 }
let version t = t.version

let add key value t =
  let id = Key.uid key in
  { bindings = Int_map.add id (B (key, value)) t.bindings; version = t.version + 1 }

let get (type a) (key : a Key.t) t : a option =
  match Int_map.find_opt (Key.uid key) t.bindings with
  | None -> None
  | Some (B (stored_key, value)) -> (
      match Type.Id.provably_equal (Key.id key) (Key.id stored_key) with
      | Some Type.Equal -> Some value
      | None -> None)

let mem key t = Int_map.mem (Key.uid key) t.bindings

let get_exn key t =
  match get key t with
  | Some value -> value
  | None -> invalid_arg (Fmt.str "Charamel_huh.Results.get_exn: %s unset" (Key.name key))
