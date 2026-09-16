type t = { name : string; description : string; schema : Jsont.json }

let v ~name ~description ~schema = { name; description; schema }
let pp ppf { name; description; _ } = Fmt.pf ppf "@[<h>%s — %s@]" name description
