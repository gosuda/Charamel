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
    (List.map
       (fun dir -> Filename.concat dir "crush.json")
       [ "/home/test/.config/crush"; "/repo"; "/repo/src"; "/repo/src/lib" ])
    paths;
  let paths =
    Config.search_paths ~cwd:"/repo/src" ~git_root:None
      ~home_config:"/home/test/crush.json"
  in
  Alcotest.(check (list string))
    "home and cwd without git root"
    [ "/home/test/crush.json"; Filename.concat "/repo/src" "crush.json" ]
    paths

let write path body =
  Test_tools_test_support.mkdir_p (Filename.dirname path);
  let channel =
    open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] 0o600 path
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

let load_or_fail ~env cwd =
  match await (Config.load ~fs_root:"/" ~env ~cwd) with
  | Error error -> Alcotest.failf "load failed: %a" Config.pp_error error
  | Ok loaded -> loaded

let grant_or_fail ~env files =
  match await (Config.grant ~fs_root:"/" ~env files) with
  | Error error -> Alcotest.failf "grant failed: %a" Config.pp_error error
  | Ok () -> ()

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
      let first = load_or_fail ~env:env_lookup nested in
      Alcotest.(check (list string))
        "the root project file waits for trust"
        [ Filename.concat (Unix.realpath project) "crush.json" ]
        (List.map
           (fun (file : Config.project_file) -> file.Config.path)
           first.Config.untrusted);
      grant_or_fail ~env:env_lookup first.Config.untrusted;
      let { Config.config; files; untrusted } = load_or_fail ~env:env_lookup nested in
      Alcotest.(check int) "three files contribute" 3 (List.length files);
      Alcotest.(check int) "every gated file is trusted" 0 (List.length untrusted);
      Alcotest.(check bool) "root scalar wins" true config.Config.options.Config.debug;
      Alcotest.(check string)
        "options values are not expanded" "$DATA" config.Config.options.Config.data_dir;
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
            "environment expands api key" (Some "secret-token") provider.Config.api_key)

let user_config =
  {|{"permissions":{"allowed_tools":["read"]},"hooks":[{"event":"stop","command":"user-hook"}],"options":{"auto_lsp":false}}|}

let gated_project =
  {|{"providers":{"relay":{"type":"anthropic","base_url":"http://127.0.0.1:9"}},"mcp":{"probe":{"command":"probe-mcp"}},"lsp":{"probe":{"command":"probe-lsp"}},"hooks":[{"event":"session_start","command":"probe-hook"}],"permissions":{"allowed_tools":["bash"],"deny":["edit"]},"options":{"auto_lsp":true,"debug":true},"context_paths":["PROJECT.md"]}|}

let tree_env ~root ~xdg = function
  | "XDG_CONFIG_HOME" -> Some xdg
  | "HOME" -> Some (Filename.concat root "home")
  | _ -> None

let hook_commands (config : Config.t) =
  List.map (fun (hook : Config.hook) -> hook.Config.command) config.Config.hooks

let check_gated_config ~trusted (config : Config.t) =
  let gated untrusted_value trusted_value =
    if trusted then trusted_value else untrusted_value
  in
  let state = if trusted then "trusted" else "untrusted" in
  Alcotest.(check (list string))
    (state ^ " MCP servers") (gated [] [ "probe" ])
    (List.map fst config.Config.mcp);
  Alcotest.(check (list string))
    (state ^ " LSP servers") (gated [] [ "probe" ])
    (List.map fst config.Config.lsp);
  Alcotest.(check (list string))
    (state ^ " providers") (gated [] [ "relay" ])
    (List.map fst config.Config.providers);
  Alcotest.(check (list string))
    (state ^ " hooks")
    (gated [ "user-hook" ] [ "user-hook"; "probe-hook" ])
    (hook_commands config);
  Alcotest.(check (list string))
    (state ^ " allowed tools")
    (gated [ "read" ] [ "read"; "bash" ])
    config.Config.permissions.Config.allowed_tools;
  Alcotest.(check bool)
    (state ^ " auto_lsp") (gated false true) config.Config.options.Config.auto_lsp;
  Alcotest.(check (list string))
    "project deny applies" [ "edit" ] config.Config.permissions.Config.deny;
  Alcotest.(check bool) "project debug applies" true config.Config.options.Config.debug;
  Alcotest.(check bool)
    "project context path applies" true
    (List.mem "PROJECT.md" config.Config.context_paths)

let sha256 text = Digestif.SHA256.(digest_string text |> to_hex)

let check_project_trust () =
  with_config_tree (fun ~root ~xdg ~project ~nested ->
      let env = tree_env ~root ~xdg in
      let project_file = project ^ "/crush.json" in
      write (xdg ^ "/crush/crush.json") user_config;
      write project_file gated_project;
      let first = load_or_fail ~env nested in
      check_gated_config ~trusted:false first.Config.config;
      (match first.Config.untrusted with
      | [ file ] ->
          Alcotest.(check string)
            "canonical path"
            (Filename.concat (Unix.realpath project) "crush.json")
            file.Config.path;
          Alcotest.(check string)
            "digest of the bytes" (sha256 gated_project) file.Config.digest;
          Alcotest.(check (list string))
            "gated members"
            [
              "providers";
              "mcp";
              "lsp";
              "hooks";
              "permissions.allowed_tools";
              "options.auto_lsp";
            ]
            file.Config.gated
      | files -> Alcotest.failf "expected one untrusted file, got %d" (List.length files));
      Alcotest.(check bool)
        "loading writes no trust record" false
        (Sys.file_exists (xdg ^ "/crush/trust.json"));
      grant_or_fail ~env first.Config.untrusted;
      let store = xdg ^ "/crush/trust.json" in
      Alcotest.(check bool)
        "the record lives in the config home" true (Sys.file_exists store);
      if not Sys.win32 then
        Alcotest.(check int)
          "the record is private" 0o600
          ((Unix.stat store).Unix.st_perm land 0o777);
      let trusted = load_or_fail ~env nested in
      check_gated_config ~trusted:true trusted.Config.config;
      Alcotest.(check int) "nothing is untrusted" 0 (List.length trusted.Config.untrusted);
      write project_file (gated_project ^ "\n");
      let edited = load_or_fail ~env nested in
      check_gated_config ~trusted:false edited.Config.config;
      Alcotest.(check (list string))
        "an edit revokes trust"
        [ sha256 (gated_project ^ "\n") ]
        (List.map
           (fun (file : Config.project_file) -> file.Config.digest)
           edited.Config.untrusted))

let check_trust_path () =
  if Sys.win32 then Alcotest.skip ();
  with_config_tree (fun ~root ~xdg ~project ~nested:_ ->
      let env = tree_env ~root ~xdg in
      write (project ^ "/crush.json") gated_project;
      grant_or_fail ~env (load_or_fail ~env project).Config.untrusted;
      let alias = root ^ "/alias" in
      Unix.symlink project alias;
      let through_alias = load_or_fail ~env (alias ^ "/src") in
      Alcotest.(check int)
        "trust holds through a symlinked directory" 0
        (List.length through_alias.Config.untrusted);
      Alcotest.(check (list string))
        "the symlinked directory keeps the trusted servers" [ "probe" ]
        (List.map fst through_alias.Config.config.Config.mcp);
      let copy = root ^ "/copy" in
      let linked = root ^ "/linked" in
      List.iter Test_tools_test_support.mkdir_p [ copy ^ "/.git"; linked ^ "/.git" ];
      write (copy ^ "/crush.json") gated_project;
      Unix.symlink (project ^ "/crush.json") (linked ^ "/crush.json");
      Alcotest.(check int)
        "the same bytes in another directory are untrusted" 1
        (List.length (load_or_fail ~env copy).Config.untrusted);
      Alcotest.(check int)
        "a link to a trusted file from another directory is untrusted" 1
        (List.length (load_or_fail ~env linked).Config.untrusted))

let check_corrupt_trust_store () =
  with_config_tree (fun ~root ~xdg ~project ~nested ->
      let env = tree_env ~root ~xdg in
      let store = xdg ^ "/crush/trust.json" in
      write store "{not json";
      write (nested ^ "/crush.json") {|{"context_paths":["PLAIN.md"]}|};
      Alcotest.(check bool)
        "a project file without gated members needs no trust store" true
        (List.mem "PLAIN.md" (load_or_fail ~env nested).Config.config.Config.context_paths);
      write (project ^ "/crush.json") gated_project;
      match await (Config.load ~fs_root:"/" ~env ~cwd:nested) with
      | Error (`Parse (path, _)) ->
          Alcotest.(check string) "the store is named" store path
      | Error (`Io (path, message)) ->
          Alcotest.failf "unexpected I/O error %s: %s" path message
      | Ok _ -> Alcotest.fail "a corrupt trust store was read as trusting nothing")

let duplicate_members =
  {|{"permissions":{"deny":["edit"]},"permissions":{"allowed_tools":["bash"]},"hooks":[],"hooks":[{"event":"session_start","command":"probe-hook"}]}|}

let check_duplicate_gated_members () =
  with_config_tree (fun ~root ~xdg ~project:_ ~nested ->
      let env = tree_env ~root ~xdg in
      write (nested ^ "/crush.json") duplicate_members;
      match await (Config.load ~fs_root:"/" ~env ~cwd:nested) with
      | Error (`Parse _) -> ()
      | Error (`Io (path, message)) ->
          Alcotest.failf "unexpected I/O error %s: %s" path message
      | Ok loaded ->
          let config = loaded.Config.config in
          Alcotest.(check (list string))
            "a repeated permissions member stays gated" []
            config.Config.permissions.Config.allowed_tools;
          Alcotest.(check (list string))
            "a repeated hooks member stays gated" [] (hook_commands config);
          Alcotest.(check int)
            "the file is reported" 1
            (List.length loaded.Config.untrusted))

let check_invalid_config () =
  match
    Jsont_bytesrw.decode_string Config.jsont
      "{\"options\":{\"budgets\":{\"subagent_requests\":-1}}}"
  with
  | Ok _ -> Alcotest.fail "negative subagent budget was accepted"
  | Error _ -> ()

let check_unknown_record_key () =
  let expect_rejected name json =
    match Jsont_bytesrw.decode_string Config.jsont json with
    | Ok _ -> Alcotest.fail (name ^ " was accepted")
    | Error _ -> ()
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

let selected_provider models name =
  let selection =
    match name with "large" -> models.Config.large | _ -> models.Config.small
  in
  match selection with Some sel -> sel.Config.provider | None -> "<none>"

let check_withheld_model_selections () =
  with_config_tree (fun ~root ~xdg ~project ~nested ->
      let env = tree_env ~root ~xdg in
      write (xdg ^ "/crush/crush.json")
        {|{"providers":{"home":{"type":"anthropic"}},"models":{"large":{"provider":"home","model":"m"},"small":{"provider":"home","model":"m"}}}|};
      write (project ^ "/crush.json")
        {|{"providers":{"relay":{"type":"anthropic"}},"models":{"large":{"provider":"relay","model":"m"},"small":{"provider":"relay","model":"m"}}}|};
      let loaded = load_or_fail ~env nested in
      let models = loaded.Config.config.Config.models in
      Alcotest.(check string)
        "home large survives withheld provider" "home"
        (selected_provider models "large");
      Alcotest.(check string)
        "home small survives withheld provider" "home"
        (selected_provider models "small");
      let gated =
        List.concat_map
          (fun (file : Config.project_file) -> file.Config.gated)
          loaded.Config.untrusted
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("gated lists " ^ needle) true (List.mem needle gated))
        [ "providers"; "models.large"; "models.small" ];
      write (project ^ "/crush.json")
        {|{"providers":{"relay":{"type":"anthropic"}},"models":{"large":{"provider":"external","model":"m"}}}|};
      let reloaded = load_or_fail ~env nested in
      Alcotest.(check string)
        "selection naming another provider is preserved" "external"
        (selected_provider reloaded.Config.config.Config.models "large"))

let check_untrusted_auto_lsp_false_preserved () =
  with_config_tree (fun ~root ~xdg ~project:_ ~nested ->
      let env = tree_env ~root ~xdg in
      write (xdg ^ "/crush/crush.json") {|{"permissions":{"allowed_tools":["read"]}}|};
      write (nested ^ "/crush.json") {|{"options":{"auto_lsp":false}}|};
      let loaded = load_or_fail ~env nested in
      Alcotest.(check bool)
        "explicit false opt-out applies without trust" false
        loaded.Config.config.Config.options.Config.auto_lsp;
      Alcotest.(check int)
        "false opt-out needs no trust" 0
        (List.length loaded.Config.untrusted))

let check_grant_checked_validates () =
  with_config_tree (fun ~root ~xdg ~project ~nested ->
      let env = tree_env ~root ~xdg in
      write (xdg ^ "/crush/crush.json") {|{"permissions":{"allowed_tools":["read"]}}|};
      write (project ^ "/crush.json")
        {|{"hooks":[{"event":"session_start","command":"${TEAM_HOOK}"}]}|};
      let loaded = load_or_fail ~env nested in
      Alcotest.(check int) "hook waits for trust" 1 (List.length loaded.Config.untrusted);
      (match
         await
           (Config.grant_checked ~fs_root:"/" ~env ~cwd:nested loaded.Config.untrusted)
       with
      | Ok () -> Alcotest.fail "grant_checked accepted an invalid hook"
      | Error _ -> ());
      Alcotest.(check bool)
        "failing grant writes no trust record" false
        (Sys.file_exists (xdg ^ "/crush/trust.json"));
      let env2 = function "TEAM_HOOK" -> Some "echo hi" | name -> env name in
      (match
         await
           (Config.grant_checked ~fs_root:"/" ~env:env2 ~cwd:nested
              loaded.Config.untrusted)
       with
      | Error error -> Alcotest.failf "valid grant failed: %a" Config.pp_error error
      | Ok () -> ());
      Alcotest.(check bool)
        "valid grant writes the store" true
        (Sys.file_exists (xdg ^ "/crush/trust.json")))

let check_grant_checked_validates_layers () =
  with_config_tree (fun ~root ~xdg ~project ~nested ->
      let env = tree_env ~root ~xdg in
      write (xdg ^ "/crush/crush.json") {|{"permissions":{"allowed_tools":["read"]}}|};
      (* The child override is valid, but the parent declaration it masks expands
         to an empty command: granting from the child must not save the parent. *)
      write (project ^ "/crush.json") {|{"lsp":{"team":{"command":"${TEAM_LSP}"}}}|};
      write (nested ^ "/crush.json") {|{"lsp":{"team":{"command":"echo hi"}}}|};
      let loaded = load_or_fail ~env nested in
      Alcotest.(check int)
        "parent and child wait for trust" 2
        (List.length loaded.Config.untrusted);
      (match
         await
           (Config.grant_checked ~fs_root:"/" ~env ~cwd:nested loaded.Config.untrusted)
       with
      | Ok () -> Alcotest.fail "grant_checked accepted a masked invalid parent"
      | Error _ -> ());
      Alcotest.(check bool)
        "masking grant writes no trust record" false
        (Sys.file_exists (xdg ^ "/crush/trust.json"));
      let env2 = function "TEAM_LSP" -> Some "echo parent" | name -> env name in
      (match
         await
           (Config.grant_checked ~fs_root:"/" ~env:env2 ~cwd:nested
              loaded.Config.untrusted)
       with
      | Error error -> Alcotest.failf "valid grant failed: %a" Config.pp_error error
      | Ok () -> ());
      Alcotest.(check bool)
        "valid layered grant writes the store" true
        (Sys.file_exists (xdg ^ "/crush/trust.json")))

let check_dangling_model_selection_falls_back () =
  with_config_tree (fun ~root ~xdg ~project ~nested ->
      let env = tree_env ~root ~xdg in
      write (xdg ^ "/crush/crush.json")
        {|{"providers":{"home":{"type":"anthropic"}},"models":{"large":{"provider":"home","model":"m"}}}|};
      write (project ^ "/crush.json") {|{"providers":{"relay":{"type":"anthropic"}}}|};
      (* The nested file holds no gated settings of its own, only a selection
         that names the withheld parent provider. *)
      write (nested ^ "/crush.json")
        {|{"models":{"large":{"provider":"relay","model":"m"}}}|};
      let loaded = load_or_fail ~env nested in
      let models = loaded.Config.config.Config.models in
      Alcotest.(check string)
        "home large survives the dangling selection" "home"
        (selected_provider models "large"))

let cases =
  [
    Test_tools_test_support.case "environment expansion" `Quick check_expand;
    Test_tools_test_support.case "search path ordering" `Quick check_search_paths;
    Test_tools_test_support.case "layered config" `Quick check_layered_load;
    Test_tools_test_support.case "project config waits for trust" `Quick
      check_project_trust;
    Test_tools_test_support.case "trust follows the canonical path" `Quick
      check_trust_path;
    Test_tools_test_support.case "corrupt trust store fails closed" `Quick
      check_corrupt_trust_store;
    Test_tools_test_support.case "repeated gated members stay gated" `Quick
      check_duplicate_gated_members;
    Test_tools_test_support.case "invalid config" `Quick check_invalid_config;
    Test_tools_test_support.case "unknown record key" `Quick check_unknown_record_key;
    Test_tools_test_support.case "opaque map keys" `Quick check_opaque_map_keys;
    Test_tools_test_support.case "JSON presence helpers" `Quick check_jsonx_presence;
    Test_tools_test_support.case "withheld providers drop their selections" `Quick
      check_withheld_model_selections;
    Test_tools_test_support.case "untrusted auto_lsp false is preserved" `Quick
      check_untrusted_auto_lsp_false_preserved;
    Test_tools_test_support.case "grant_checked validates before saving" `Quick
      check_grant_checked_validates;
    Test_tools_test_support.case "grant_checked validates layers" `Quick
      check_grant_checked_validates_layers;
    Test_tools_test_support.case "dangling model selection falls back" `Quick
      check_dangling_model_selection_falls_back;
  ]
