type t = Any : 'a Type.Id.t * 'a -> t

let inject id message = Any (id, message)

let project (type a) (id : a Type.Id.t) (Any (other_id, message)) : a option =
  match Type.Id.provably_equal id other_id with
  | Some Type.Equal -> Some message
  | None -> None
