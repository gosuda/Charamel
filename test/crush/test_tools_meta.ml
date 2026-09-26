module Tool = Crush_core.Tool
module Tools_meta = Crush_core.Tools_meta

let names_and_modes () =
  let names = List.map (fun (tool : Tool.t) -> tool.Tool.name) Tools_meta.all in
  Alcotest.(check (list string))
    "metadata order"
    [ "todos"; "question"; "crush_info"; "crush_logs" ]
    names;
  Alcotest.(check bool) "todos are mutable" false Tools_meta.todos.Tool.read_only;
  Alcotest.(check bool)
    "question is read-only metadata" true Tools_meta.question.Tool.read_only;
  Alcotest.(check bool) "info is read-only" true Tools_meta.crush_info.Tool.read_only;
  Alcotest.(check bool) "logs are read-only" true Tools_meta.crush_logs.Tool.read_only

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

let cases =
  [
    Test_tools_test_support.case "metadata names and modes" `Quick names_and_modes;
    Test_tools_test_support.case "metadata schemas" `Quick schemas;
  ]
