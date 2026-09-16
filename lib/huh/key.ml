type 'a t = { id : 'a Type.Id.t; name : string }

let v name = { id = Type.Id.make (); name }
let name key = key.name
let uid key = Type.Id.uid key.id
let id key = key.id
let equal a b = Type.Id.uid a.id = Type.Id.uid b.id
let pp ppf key = Fmt.string ppf key.name
