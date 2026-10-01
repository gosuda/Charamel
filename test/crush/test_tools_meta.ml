module Tool = Crush_core.Tool
module Tools_meta = Crush_core.Tools_meta

let required name schema =
  match schema with
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem "required" members with
      | Some (_, Jsont.Array (values, _)) ->
          List.exists
            (function Jsont.String (value, _) -> value = name | _ -> false)
            values
      | _ -> false)
  | _ -> false

let schemas () =
  Alcotest.(check bool)
    "todos requires todos" true
    (required "todos" Tools_meta.todos.Tool.schema);
  Alcotest.(check bool)
    "questions requires questions" true
    (required "questions" Tools_meta.question.Tool.schema);
  Alcotest.(check bool)
    "logs leaves lines optional" false
    (required "lines" Tools_meta.crush_logs.Tool.schema)

let cases = [ Test_tools_test_support.case "metadata schemas" `Quick schemas ]
