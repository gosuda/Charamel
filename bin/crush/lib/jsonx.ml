let json_of_string text = Jsont_bytesrw.decode_string Jsont.json text

let string_of_json ?(minify = true) json =
  let format = if minify then Jsont.Minify else Jsont.Indent in
  match Jsont_bytesrw.encode_string ~format Jsont.json json with
  | Ok text -> text
  | Error message -> invalid_arg (Fmt.str "JSON encoding failed: %s" message)

let decode codec text =
  match json_of_string text with
  | Error message -> Error message
  | Ok json -> Jsont.Json.decode codec json

let encode ?(minify = true) codec value =
  match Jsont.Json.encode codec value with
  | Error message -> invalid_arg (Fmt.str "JSON encoding failed: %s" message)
  | Ok json -> string_of_json ~minify json

let member name = function
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem name members with
      | None -> None
      | Some (_, value) -> Some value)
  | _ -> None

let string_member name json =
  match member name json with Some (Jsont.String (value, _)) -> Some value | _ -> None

let int_member name json =
  match member name json with
  | Some (Jsont.Number (value, _))
    when Float.is_finite value && Float.is_integer value
         && value >= float_of_int min_int
         && value <= float_of_int max_int ->
      Some (int_of_float value)
  | _ -> None

let bool_member name json =
  match member name json with Some (Jsont.Bool (value, _)) -> Some value | _ -> None
