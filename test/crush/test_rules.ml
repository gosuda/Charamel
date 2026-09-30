module Config = Crush_core.Config
module Rules = Crush_core.Rules
open Lwt_direct

let setup f =
  Test_tools_test_support.with_scratch (fun root ->
      let rules_dir = Filename.concat root ".crush/rules" in
      Test_tools_test_support.mkdir_p rules_dir;
      f root rules_dir)

let write path content = Test_tools_test_support.write_file path content

let always_and_glob_rules () =
  setup (fun root rules_dir ->
      write (rules_dir ^ "/always.md") "Always inspect before editing.\n";
      write (rules_dir ^ "/ml.md")
        "---\nglobs: [\"**/*.ml\", \"**/*.mli\"]\n---\nUse the OCaml style.\n";
      let config = { Config.default with context_paths = [ ".crush/rules" ] } in
      let rules = await (Rules.load ~fs_root:"/" ~cwd:root ~config) in
      let context = Rules.context_text rules in
      (* [Rules] reports [Path.normalize]d paths — \\ separators become /. *)
      Alcotest.check Alcotest.bool "always context" true
        (String.equal context
           ("## "
           ^ Crush_core.Path.normalize (rules_dir ^ "/always.md")
           ^ "\nAlways inspect before editing.\n\n"));
      let attached =
        Rules.attach_text rules ~touched:[ root ^ "/src/main.ml"; root ^ "/src/main.ml" ]
      in
      Alcotest.check Alcotest.bool "glob attachment" true
        (String.ends_with ~suffix:"Use the OCaml style.\n\n" attached);
      Alcotest.check Alcotest.int "one attached rule" 1
        (List.length (Rules.for_path rules (root ^ "/src/main.ml")));
      Alcotest.check Alcotest.int "nonmatching path" 0
        (List.length (Rules.for_path rules (root ^ "/src/main.py"))))

let yaml_and_empty_headers () =
  setup (fun root rules_dir ->
      write (rules_dir ^ "/empty.md") "---\n---\nNo globs means always.\n";
      write (rules_dir ^ "/yaml.md")
        "---\nglobs:\n  - \"src/**/*.ml\"\n  - \"lib/**/*.ml\"\n---\nYAML list.\n";
      let config = { Config.default with context_paths = [ ".crush/rules" ] } in
      let rules = await (Rules.load ~fs_root:"/" ~cwd:root ~config) in
      Alcotest.check Alcotest.bool "empty header is always" true
        (String.contains (Rules.context_text rules) 'N');
      Alcotest.check Alcotest.int "yaml glob matches" 1
        (List.length (Rules.for_path rules (root ^ "/src/a.ml"))))

let explicit_paths_only () =
  setup (fun root rules_dir ->
      write (root ^ "/unconfigured.md") "STRAYZ";
      write (rules_dir ^ "/configured.md") "CONFIGURED_MARKER";
      let config =
        { Config.default with context_paths = [ ".crush/rules/configured.md" ] }
      in
      let rules = await (Rules.load ~fs_root:"/" ~cwd:root ~config) in
      let context = Rules.context_text rules in
      Alcotest.check Alcotest.bool "configured source included" true
        (String.contains context 'C');
      Alcotest.check Alcotest.bool "unconfigured source excluded" false
        (String.contains context 'Z'))

let oversized_rules_are_not_indexed () =
  setup (fun root rules_dir ->
      write (rules_dir ^ "/huge.md") (String.make 65_537 'x');
      let config = { Config.default with context_paths = [ ".crush/rules" ] } in
      let rules = await (Rules.load ~fs_root:"/" ~cwd:root ~config) in
      Alcotest.check Alcotest.string "oversized rule context" ""
        (Rules.context_text rules))

let invalid_glob_is_explicit () =
  setup (fun root rules_dir ->
      write (rules_dir ^ "/bad.md") "---\nglobs: [\"[\"]\n---\nbad\n";
      let config = { Config.default with context_paths = [ ".crush/rules" ] } in
      match
        try
          ignore (await (Rules.load ~fs_root:"/" ~cwd:root ~config));
          None
        with Invalid_argument message -> Some message
      with
      | Some message ->
          Alcotest.check Alcotest.bool "invalid glob names rule" true
            (String.contains message 'b')
      | None -> Alcotest.fail "invalid glob was accepted")

let cases =
  [
    Test_tools_test_support.case "always and glob context" `Quick always_and_glob_rules;
    Test_tools_test_support.case "YAML and empty headers" `Quick yaml_and_empty_headers;
    Test_tools_test_support.case "only configured paths are loaded" `Quick
      explicit_paths_only;
    Test_tools_test_support.case "oversized rules are bounded" `Quick
      oversized_rules_are_not_indexed;
    Test_tools_test_support.case "invalid globs are explicit" `Quick
      invalid_glob_is_explicit;
  ]
