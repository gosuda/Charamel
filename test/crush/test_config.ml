module Config = Crush_core.Config
open Lwt_direct

let check_expand () =
  let env = function "NAME" -> Some "Ada" | "EMPTY" -> Some "" | _ -> None in
  Alcotest.(check string)
    "dollar variable" "hello Ada"
    (Config.expand_env ~env "hello $NAME");
  Alcotest.(check string) "braced variable" "Ada" (Config.expand_env ~env "${NAME}");
  Alcotest.(check string) "escaped dollar" "$NAME" (Config.expand_env ~env "$$NAME");
  Alcotest.(check string) "unset variable" "" (Config.expand_env ~env "$MISSING");
  Alcotest.(check string) "literal dollar" "$-" (Config.expand_env ~env "$-")

let check_search_paths () =
  let paths =
    Config.search_paths ~cwd:"/repo/src/lib" ~git_root:(Some "/repo")
      ~home_config:"/home/test/.config/crush"
  in
  Alcotest.(check (list string))
    "ordered paths"
    [
      "/home/test/.config/crush/crush.json";
      "/repo/crush.json";
      "/repo/src/crush.json";
      "/repo/src/lib/crush.json";
    ]
    paths;
  let paths =
    Config.search_paths ~cwd:"/repo/src" ~git_root:None
      ~home_config:"/home/test/crush.json"
  in
  Alcotest.(check (list string))
    "home and cwd without git root"
    [ "/home/test/crush.json"; "/repo/src/crush.json" ]
    paths

let write path body =
  Test_tools_test_support.mkdir_p (Filename.dirname path);
  let channel =
    open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_text ] 0o600 path
  in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel body)

let with_config_tree f =
  Test_tools_test_support.with_scratch (fun root ->
      let home = Filename.concat root "home" in
      let xdg = Filename.concat home "xdg" in
      let project = Filename.concat root "project" in
      let nested = Filename.concat project "src" in
      List.iter Test_tools_test_support.mkdir_p
        [ xdg ^ "/crush"; nested; project; project ^ "/.git" ];
      f ~root ~xdg ~project ~nested)

let check_layered_load () =
  with_config_tree (fun ~root ~xdg ~project ~nested ->
      let home_file = xdg ^ "/crush/crush.json" in
      let project_file = project ^ "/crush.json" in
      let nested_file = nested ^ "/crush.json" in
      write home_file
        "{\"providers\":{\"anthropic\":{\"type\":\"anthropic\",\"api_key\":\"$TOKEN\"}},\"permissions\":{\"allowed_tools\":[\"read\"]},\"options\":{\"debug\":false,\"data_dir\":\"home-data\"},\"context_paths\":[\"A.md\"]}";
      write project_file
        "{\"providers\":{\"openai\":{\"type\":\"openai\"}},\"permissions\":{\"allowed_tools\":[\"edit\"]},\"options\":{\"debug\":true},\"context_paths\":[\"B.md\"]}";
      write nested_file
        "{\"options\":{\"data_dir\":\"$DATA\"},\"context_paths\":[\"A.md\",\"C.md\"]}";
      let env_lookup name =
        match name with
        | "XDG_CONFIG_HOME" -> Some xdg
        | "HOME" -> Some (Filename.concat root "home")
        | "TOKEN" -> Some "secret-token"
        | "DATA" -> Some "project-data"
        | _ -> None
      in
      match await (Config.load ~fs_root:"/" ~env:env_lookup ~cwd:nested) with
      | Error error -> Alcotest.failf "layered load failed: %a" Config.pp_error error
      | Ok (config, files) -> (
          Alcotest.(check int) "three files contribute" 3 (List.length files);
          Alcotest.(check bool) "root scalar wins" true config.Config.options.Config.debug;
          Alcotest.(check string)
            "options values are not expanded" "$DATA"
            config.Config.options.Config.data_dir;
          Alcotest.(check (list string))
            "list merge deduplicates in order" [ "A.md"; "B.md"; "C.md" ]
            config.Config.context_paths;
          Alcotest.(check (list string))
            "permission lists concatenate" [ "read"; "edit" ]
            config.Config.permissions.Config.allowed_tools;
          match List.assoc_opt "anthropic" config.Config.providers with
          | None -> Alcotest.fail "home provider missing"
          | Some provider ->
              Alcotest.(check (option string))
                "environment expands api key" (Some "secret-token")
                provider.Config.api_key))

let check_invalid_config () =
  match
    Jsont_bytesrw.decode_string Config.jsont
      "{\"options\":{\"budgets\":{\"subagent_requests\":-1}}}"
  with
  | Ok _ -> Alcotest.fail "negative subagent budget was accepted"
  | Error message ->
      Alcotest.(check bool) "validation error is reported" true (String.length message > 0)

let check_unknown_record_key () =
  let expect_rejected name json =
    match Jsont_bytesrw.decode_string Config.jsont json with
    | Ok _ -> Alcotest.fail (name ^ " was accepted")
    | Error message ->
        Alcotest.(check bool) (name ^ " reports an error") true (String.length message > 0)
  in
  expect_rejected "unknown top-level key" "{\"typo\":true}";
  expect_rejected "unknown options key" "{\"options\":{\"debug\":true,\"debg\":true}}"

let check_opaque_map_keys () =
  let json =
    "{\"providers\":{\"demo\":{\"type\":\"openai_compatible\",\"headers\":{\"X-Test\":\"ok\"}}}}"
  in
  match Jsont_bytesrw.decode_string Config.jsont json with
  | Error message -> Alcotest.failf "opaque header key was rejected: %s" message
  | Ok config -> (
      match List.assoc_opt "demo" config.Config.providers with
      | None -> Alcotest.fail "provider was not decoded"
      | Some provider ->
          Alcotest.(check (list (pair string string)))
            "header key remains arbitrary"
            [ ("X-Test", "ok") ]
            provider.Config.headers)

let check_jsonx_presence () =
  match Crush_core.Jsonx.json_of_string "{\"present\":null,\"number\":3}" with
  | Error message -> Alcotest.failf "JSON parse failed: %s" message
  | Ok json ->
      Alcotest.(check bool)
        "null member is present" true
        (Option.is_some (Crush_core.Jsonx.member "present" json));
      Alcotest.(check bool)
        "absent member is absent" true
        (Option.is_none (Crush_core.Jsonx.member "missing" json));
      Alcotest.(check (option string))
        "null is not a string" None
        (Crush_core.Jsonx.string_member "present" json);
      Alcotest.(check (option int))
        "integer helper" (Some 3)
        (Crush_core.Jsonx.int_member "number" json)

let cases =
  [
    Test_tools_test_support.case "environment expansion" `Quick check_expand;
    Test_tools_test_support.case "search path ordering" `Quick check_search_paths;
    Test_tools_test_support.case "layered config" `Quick check_layered_load;
    Test_tools_test_support.case "invalid config" `Quick check_invalid_config;
    Test_tools_test_support.case "unknown record key" `Quick check_unknown_record_key;
    Test_tools_test_support.case "opaque map keys" `Quick check_opaque_map_keys;
    Test_tools_test_support.case "JSON presence helpers" `Quick check_jsonx_presence;
  ]
