module Config = Crush_core.Config
module Rules = Crush_core.Rules

let remove_tree path =
  let rec remove path =
    match try Some (Unix.lstat path) with Unix.Unix_error _ -> None with
    | None -> ()
    | Some stat -> (
        match stat.Unix.st_kind with
        | Unix.S_DIR ->
            Array.iter
              (fun entry -> remove (Filename.concat path entry))
              (Sys.readdir path);
            Unix.rmdir path
        | _ -> Sys.remove path)
  in
  remove path

let write fs path content =
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(fs / path) content

let setup f =
  Eio_main.run (fun env ->
      let root =
        Filename.concat
          (Filename.get_temp_dir_name ())
          ("crush-rules-" ^ string_of_int (Unix.getpid ()))
      in
      remove_tree root;
      Unix.mkdir root 0o700;
      let rules_dir = Filename.concat root ".crush/rules" in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / rules_dir);
      Fun.protect (fun () -> f env root rules_dir) ~finally:(fun () -> remove_tree root))

let always_and_glob_rules () =
  setup (fun env root rules_dir ->
      let fs = Eio.Stdenv.fs env in
      write fs (rules_dir ^ "/always.md") "Always inspect before editing.\n";
      write fs (rules_dir ^ "/ml.md")
        "---\nglobs: [\"**/*.ml\", \"**/*.mli\"]\n---\nUse the OCaml style.\n";
      let config = { Config.default with context_paths = [ ".crush/rules" ] } in
      let rules = Rules.load ~fs ~cwd:root ~config in
      let context = Rules.context_text rules in
      Alcotest.check Alcotest.bool "always context" true
        (String.equal context
           ("## " ^ rules_dir ^ "/always.md\nAlways inspect before editing.\n\n"));
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
  setup (fun env root rules_dir ->
      let fs = Eio.Stdenv.fs env in
      write fs (rules_dir ^ "/empty.md") "---\n---\nNo globs means always.\n";
      write fs (rules_dir ^ "/yaml.md")
        "---\nglobs:\n  - \"src/**/*.ml\"\n  - \"lib/**/*.ml\"\n---\nYAML list.\n";
      let config = { Config.default with context_paths = [ ".crush/rules" ] } in
      let rules = Rules.load ~fs ~cwd:root ~config in
      Alcotest.check Alcotest.bool "empty header is always" true
        (String.contains (Rules.context_text rules) 'N');
      Alcotest.check Alcotest.int "yaml glob matches" 1
        (List.length (Rules.for_path rules (root ^ "/src/a.ml"))))

let explicit_paths_only () =
  setup (fun env root rules_dir ->
      let fs = Eio.Stdenv.fs env in
      write fs (root ^ "/unconfigured.md") "STRAYZ";
      write fs (rules_dir ^ "/configured.md") "CONFIGURED_MARKER";
      let config =
        { Config.default with context_paths = [ ".crush/rules/configured.md" ] }
      in
      let rules = Rules.load ~fs ~cwd:root ~config in
      let context = Rules.context_text rules in
      Alcotest.check Alcotest.bool "configured source included" true
        (String.contains context 'C');
      Alcotest.check Alcotest.bool "unconfigured source excluded" false
        (String.contains context 'Z'))

let oversized_rules_are_not_indexed () =
  setup (fun env root rules_dir ->
      let fs = Eio.Stdenv.fs env in
      write fs (rules_dir ^ "/huge.md") (String.make 65_537 'x');
      let config = { Config.default with context_paths = [ ".crush/rules" ] } in
      let rules = Rules.load ~fs ~cwd:root ~config in
      Alcotest.check Alcotest.string "oversized rule context" ""
        (Rules.context_text rules))

let invalid_glob_is_explicit () =
  setup (fun env root rules_dir ->
      let fs = Eio.Stdenv.fs env in
      write fs (rules_dir ^ "/bad.md") "---\nglobs: [\"[\"]\n---\nbad\n";
      let config = { Config.default with context_paths = [ ".crush/rules" ] } in
      match
        try
          ignore (Rules.load ~fs ~cwd:root ~config);
          None
        with Invalid_argument message -> Some message
      with
      | Some message ->
          Alcotest.check Alcotest.bool "invalid glob names rule" true
            (String.contains message 'b')
      | None -> Alcotest.fail "invalid glob was accepted")

let cases =
  [
    Alcotest.test_case "always and glob context" `Quick always_and_glob_rules;
    Alcotest.test_case "YAML and empty headers" `Quick yaml_and_empty_headers;
    Alcotest.test_case "only configured paths are loaded" `Quick explicit_paths_only;
    Alcotest.test_case "oversized rules are bounded" `Quick
      oversized_rules_are_not_indexed;
    Alcotest.test_case "invalid globs are explicit" `Quick invalid_glob_is_explicit;
  ]
