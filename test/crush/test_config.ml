module Config = Crush_core.Config

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

let write path body = Eio.Path.save ~create:(`Or_truncate 0o600) path body

let with_config_tree f =
  Eio_main.run (fun runtime ->
      let root = Fmt.str "/tmp/crush-config-%d-%d" (Unix.getpid ()) (Random.bits ()) in
      let home = Filename.concat root "home" in
      let xdg = Filename.concat home "xdg" in
      let project = Filename.concat root "project" in
      let nested = Filename.concat project "src" in
      List.iter
        (fun path ->
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(runtime#fs / path))
        [ xdg ^ "/crush"; nested; project ^ "/.git" ];
      let cleanup () = Eio.Path.rmtree ~missing_ok:true Eio.Path.(runtime#fs / root) in
      Fun.protect ~finally:cleanup (fun () -> f runtime ~root ~xdg ~project ~nested))

let check_layered_load () =
  with_config_tree (fun runtime ~root ~xdg ~project ~nested ->
      let home_file = Eio.Path.(runtime#fs / xdg / "crush" / "crush.json") in
      let project_file = Eio.Path.(runtime#fs / project / "crush.json") in
      let nested_file = Eio.Path.(runtime#fs / nested / "crush.json") in
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
      match Config.load ~fs:runtime#fs ~env:env_lookup ~cwd:nested with
      | Error error -> Alcotest.failf "layered load failed: %a" Config.pp_error error
      | Ok (config, files) -> (
          Alcotest.(check int) "three files contribute" 3 (List.length files);
          Alcotest.(check bool) "root scalar wins" true config.options.debug;
          Alcotest.(check string)
            "options values are not expanded" "$DATA" config.options.data_dir;
          Alcotest.(check (list string))
            "list merge deduplicates in order" [ "A.md"; "B.md"; "C.md" ]
            config.context_paths;
          Alcotest.(check (list string))
            "permission lists concatenate" [ "read"; "edit" ]
            config.permissions.allowed_tools;
          match List.assoc_opt "anthropic" config.providers with
          | None -> Alcotest.fail "home provider missing"
          | Some provider ->
              Alcotest.(check (option string))
                "environment expands api key" (Some "secret-token") provider.api_key))

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
      match List.assoc_opt "demo" config.providers with
      | None -> Alcotest.fail "provider was not decoded"
      | Some provider ->
          Alcotest.(check (list (pair string string)))
            "header key remains arbitrary"
            [ ("X-Test", "ok") ]
            provider.headers)

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
    Alcotest.test_case "environment expansion" `Quick check_expand;
    Alcotest.test_case "search path ordering" `Quick check_search_paths;
    Alcotest.test_case "layered config" `Quick check_layered_load;
    Alcotest.test_case "invalid config" `Quick check_invalid_config;
    Alcotest.test_case "unknown record key" `Quick check_unknown_record_key;
    Alcotest.test_case "opaque map keys" `Quick check_opaque_map_keys;
    Alcotest.test_case "JSON presence helpers" `Quick check_jsonx_presence;
  ]
