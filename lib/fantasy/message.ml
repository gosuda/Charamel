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

let role_name = function
  | System -> "system"
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool"

let pp_part ppf = function
  | Text s -> Fmt.pf ppf "text %d" (String.length s)
  | Reasoning { text; signature } -> (
      match signature with
      | Some _ -> Fmt.pf ppf "reasoning %d (signed)" (String.length text)
      | None -> Fmt.pf ppf "reasoning %d" (String.length text))
  | File { mime; _ } -> Fmt.pf ppf "file %s" mime
  | Tool_call { id; name; _ } -> Fmt.pf ppf "call %s/%s" name id
  | Tool_result { id; _ } -> Fmt.pf ppf "result %s" id

let pp ppf { role; parts } =
  Fmt.pf ppf "@[<h>%s(" (role_name role);
  let first = ref true in
  let sep () = if !first then first := false else Fmt.string ppf "; " in
  List.iter
    (fun part ->
      sep ();
      pp_part ppf part)
    parts;
  Fmt.string ppf ")@]"
