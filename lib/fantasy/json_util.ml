let oopt (j : Jsont.json) (n : string) : Jsont.json option =
  match j with
  | Jsont.Object (o, _) -> (
      match Jsont.Json.find_mem n o with Some (_, v) -> Some v | None -> None)
  | _ -> None

let is_object j = Jsont.Json.sort j = Jsont.Sort.Object

let has_error j =
  match oopt j "error" with
  | Some value -> Jsont.Json.sort value <> Jsont.Sort.Null
  | None -> false

let string_mem j n =
  match oopt j n with Some (Jsont.String (s, _)) -> Some s | Some _ | None -> None

let int_mem j n =
  match oopt j n with Some (Jsont.Number (v, _)) -> int_of_float v | Some _ | None -> 0

let int_option_mem j n =
  match oopt j n with
  | Some (Jsont.Number (v, _)) -> Some (int_of_float v)
  | Some _ | None -> None

let bool_mem j n =
  match oopt j n with Some (Jsont.Bool (b, _)) -> b | Some _ | None -> false

let objects_of_json j =
  match j with Jsont.Array (l, _) when List.for_all is_object l -> Some l | _ -> None

let array_of = function Jsont.Array (items, _) -> Some items | _ -> None

let strings_of_json j =
  match Jsont.Json.decode Jsont.(list string) j with Ok l -> Some l | Error _ -> None

let json_of_string s = Jsont_bytesrw.decode_string Jsont.json s

(* The minified writer is used because indented output would put newlines
   inside tool argument strings. *)
let string_of_json (j : Jsont.json) =
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify Jsont.json j with
  | Ok s -> s
  | Error _ -> invalid_arg "fantasy codec received an unencodable JSON value"

let n = Jsont.Json.name
let str = Jsont.Json.string
let obj = Jsont.Json.object'
let arr = Jsont.Json.list
let num = Jsont.Json.number
let bool = Jsont.Json.bool
let int = Jsont.Json.int
