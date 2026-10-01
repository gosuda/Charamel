module Tools_agent = Crush_core.Tools_agent

let member name value =
  match value with
  | Jsont.Object (members, _) -> Option.map snd (Jsont.Json.find_mem name members)
  | _ -> None

let has_property name schema =
  match member "properties" schema with
  | Some (Jsont.Object (members, _)) -> Option.is_some (Jsont.Json.find_mem name members)
  | _ -> false

let has_default name expected schema =
  match member "properties" schema with
  | Some (Jsont.Object (members, _)) -> (
      match Jsont.Json.find_mem name members with
      | Some (_, Jsont.Object (fields, _)) -> (
          match Jsont.Json.find_mem "default" fields with
          | Some (_, Jsont.Number (value, _)) -> int_of_float value = expected
          | _ -> false)
      | _ -> false)
  | _ -> false

let schema_contract () =
  let schema = Tools_agent.agent.Crush_core.Tool.schema in
  Alcotest.(check bool)
    "agent schema is an object" true
    (match schema with Jsont.Object _ -> true | _ -> false);
  Alcotest.(check bool) "single prompt property" true (has_property "prompt" schema);
  Alcotest.(check bool) "task array property" true (has_property "tasks" schema);
  Alcotest.(check bool) "bounded default" true (has_default "max_active" 8 schema)

let cases = [ Test_tools_test_support.case "agent schema" `Quick schema_contract ]
