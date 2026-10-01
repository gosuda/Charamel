type t = { name : string; description : string; schema : Jsont.json }

let v ~name ~description ~schema = { name; description; schema }
