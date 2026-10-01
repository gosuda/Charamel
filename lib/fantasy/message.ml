type role = System | User | Assistant | Tool

type part =
  | Text of string
  | Reasoning of { text : string; signature : string option }
  | File of { mime : string; data : string; name : string option }
  | Tool_call of { id : string; name : string; input : Jsont.json }
  | Tool_result of {
      id : string;
      name : string;
      output : [ `Text of string | `Error of string | `Media of string * string ];
    }

type t = { role : role; parts : part list }

let text role s = { role; parts = [ Text s ] }

let tool_results results =
  let fold acc (id, name, output) = Tool_result { id; name; output } :: acc in
  let parts = List.fold_left fold [] results in
  { role = Tool; parts = List.rev parts }
