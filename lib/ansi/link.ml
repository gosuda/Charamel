type t = { url : string; params : (string * string) list }

let parameter (key, value) =
  Payload.check ~what:"Link.osc8" key;
  Payload.check ~what:"Link.osc8" value;
  if
    key = "" || String.contains key '=' || String.contains key ':'
    || String.contains key ';' || String.contains value ':' || String.contains value ';'
  then invalid_arg "Link.osc8: invalid parameter";
  key ^ "=" ^ value

let osc8 = function
  | None -> "\x1b]8;;\x07"
  | Some { url; params } ->
      if url = "" then invalid_arg "Link.osc8: empty URL";
      Payload.check ~what:"Link.osc8" url;
      let params = String.concat ":" (List.map parameter params) in
      "\x1b]8;" ^ params ^ ";" ^ url ^ "\x07"
