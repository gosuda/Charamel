module Tool = Crush_core.Tool

let check_json_field name expected schema =
  match schema with
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem name members with
      | Some (_, value) ->
          Alcotest.check Alcotest.bool name true (Jsont.Json.equal expected value)
      | None -> Alcotest.failf "schema has no %s field" name)
  | _ -> Alcotest.fail "schema is not an object"

let output_case () =
  let output = Tool.ok "ready" in
  match Tool.to_result output with
  | `Text value -> Alcotest.(check string) "successful content" "ready" value
  | `Error _ -> Alcotest.fail "successful output became an error"

let failure_case () =
  let output = Tool.fail "broken" in
  match Tool.to_result output with
  | `Error value -> Alcotest.(check string) "error content" "broken" value
  | `Text _ -> Alcotest.fail "error output became text"

let schema_case () =
  let schema =
    Tool.schema_object ~required:[ "path" ]
      [ ("path", Tool.s_string ~desc:"A path" ()); ("limit", Tool.s_int ~default:20 ()) ]
  in
  check_json_field "type" (Jsont.Json.string "object") schema;
  check_json_field "required" (Jsont.Json.list [ Jsont.Json.string "path" ]) schema;
  let properties =
    match schema with
    | Jsont.Object (members, _) -> (
        match Jsont.Json.find_mem "properties" members with
        | Some (_, Jsont.Object (fields, _)) -> fields
        | _ -> Alcotest.fail "schema properties is not an object")
    | _ -> Alcotest.fail "schema is not an object"
  in
  match Jsont.Json.find_mem "limit" properties with
  | Some (_, limit) -> check_json_field "default" (Jsont.Json.int 20) limit
  | None -> Alcotest.fail "schema has no limit property"

let decode_case () =
  let value = Jsont.Json.string "hello" in
  match Tool.decode Jsont.string value with
  | Ok decoded -> Alcotest.(check string) "decoded string" "hello" decoded
  | Error _ -> Alcotest.fail "valid JSON was rejected"

let invalid_decode_case () =
  match Tool.decode Jsont.string (Jsont.Json.int 1) with
  | Error (`Invalid_input message) ->
      Alcotest.(check bool) "diagnostic is non-empty" true (String.length message > 0)
  | Error
      ((`Denied _ | `Not_found _ | `Unavailable _ | `Io _ | `Timeout _ | `Aborted) as
       error) ->
      Alcotest.failf "decode reported %a for invalid input" Tool.pp_error error
  | Ok _ -> Alcotest.fail "invalid JSON was accepted"

let fantasy_case () =
  let tool =
    {
      Tool.name = "read";
      description = "Read a file";
      schema = Tool.schema_object [ ("path", Tool.s_string ()) ];
      read_only = true;
      run = (fun _ _ -> Ok (Tool.ok "done"));
    }
  in
  let converted = Tool.to_fantasy tool in
  Alcotest.(check string) "fantasy tool name" "read" converted.Charamel_fantasy.Tool.name;
  Alcotest.(check string)
    "fantasy tool description" "Read a file" converted.Charamel_fantasy.Tool.description

let cases =
  [
    Alcotest.test_case "ok output" `Quick output_case;
    Alcotest.test_case "failed output" `Quick failure_case;
    Alcotest.test_case "schema helpers" `Quick schema_case;
    Alcotest.test_case "decode valid JSON" `Quick decode_case;
    Alcotest.test_case "decode invalid JSON" `Quick invalid_decode_case;
    Alcotest.test_case "fantasy conversion" `Quick fantasy_case;
  ]
