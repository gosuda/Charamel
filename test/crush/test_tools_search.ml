module Tool = Crush_core.Tool
module Tools_search = Crush_core.Tools_search
open Test_tools_test_support

let with_context =
  Test_tools_test_support.with_context ~temp_prefix:"crush-tools-search"
    ~allowed_tools:[ "ls"; "glob"; "grep" ]

let check_ls_skips_generated_trees () =
  with_context @@ fun env root context ->
  write_file env (Filename.concat root "src/a.ml") "let needle = 1\n";
  write_file env (Filename.concat root ".env") "secret\n";
  write_file env (Filename.concat root ".git/ignored.ml") "ignored\n";
  write_file env (Filename.concat root "_build/generated.ml") "generated\n";
  write_file env (Filename.concat root "node_modules/dependency.ml") "dependency\n";
  let output = run_tool Tools_search.ls context (json_object []) in
  Alcotest.(check bool)
    "listing includes source file" true
    (Test_support.contains ~needle:"a.ml" ~haystack:output.Tool.content);
  Alcotest.(check bool)
    "listing skips hidden entries" false
    (Test_support.contains ~needle:".env" ~haystack:output.Tool.content);
  Alcotest.(check bool)
    "listing skips .git" false
    (Test_support.contains ~needle:"ignored.ml" ~haystack:output.Tool.content);
  Alcotest.(check bool)
    "listing skips build output" false
    (Test_support.contains ~needle:"generated.ml" ~haystack:output.Tool.content);
  Alcotest.(check bool)
    "listing skips node_modules" false
    (Test_support.contains ~needle:"dependency.ml" ~haystack:output.Tool.content)

let check_glob_and_direct_hidden_path () =
  with_context @@ fun env root context ->
  write_file env (Filename.concat root "src/a.ml") "a\n";
  write_file env (Filename.concat root "src/b.ml") "b\n";
  write_file env (Filename.concat root ".env") "secret\n";
  write_file env (Filename.concat root ".git/ignored.ml") "ignored\n";
  let output =
    run_tool Tools_search.glob context
      (json_object [ ("pattern", Jsont.Json.string "**/*.ml") ])
  in
  Alcotest.(check bool)
    "glob finds source files" true
    (Test_support.contains ~needle:"src/a.ml" ~haystack:output.Tool.content);
  Alcotest.(check bool)
    "glob finds all source files" true
    (Test_support.contains ~needle:"src/b.ml" ~haystack:output.Tool.content);
  Alcotest.(check bool)
    "glob skips generated hidden trees" false
    (Test_support.contains ~needle:"ignored.ml" ~haystack:output.Tool.content);
  let direct =
    run_tool Tools_search.glob context
      (json_object [ ("pattern", Jsont.Json.string ".env") ])
  in
  Alcotest.(check bool)
    "explicit hidden glob finds the entry" true
    (Test_support.contains ~needle:".env" ~haystack:direct.Tool.content)

let check_grep_literal_regex_include_and_binary () =
  with_context @@ fun env root context ->
  write_file env (Filename.concat root "src/a.ml") "let needle = 1\nlet other = 2\n";
  write_file env (Filename.concat root "src/b.txt") "needle in text\n";
  write_file env (Filename.concat root ".env") "needle secret\n";
  write_file env (Filename.concat root "src/binary.bin") "needle\000hidden\n";
  write_file env (Filename.concat root ".git/ignored.ml") "needle ignored\n";
  let literal =
    run_tool Tools_search.grep context
      (json_object
         [
           ("pattern", Jsont.Json.string "needle");
           ("literal", Jsont.Json.bool true);
           ("include", Jsont.Json.string "*.ml");
         ])
  in
  Alcotest.(check bool)
    "literal grep includes matching source" true
    (Test_support.contains ~needle:"src/a.ml:1:let needle = 1"
       ~haystack:literal.Tool.content);
  Alcotest.(check bool)
    "include filters nonmatching basenames" false
    (Test_support.contains ~needle:"src/b.txt" ~haystack:literal.Tool.content);
  Alcotest.(check bool)
    "grep skips binary files" false
    (Test_support.contains ~needle:"binary.bin" ~haystack:literal.Tool.content);
  Alcotest.(check bool)
    "grep skips hidden trees" false
    (Test_support.contains ~needle:"ignored.ml" ~haystack:literal.Tool.content);
  let regex =
    run_tool Tools_search.grep context
      (json_object [ ("pattern", Jsont.Json.string "needle.*text") ])
  in
  Alcotest.(check bool)
    "regex grep matches text" true
    (Test_support.contains ~needle:"src/b.txt:1" ~haystack:regex.Tool.content)

let check_grep_limit_footer () =
  with_context @@ fun env root context ->
  write_file env (Filename.concat root "a.txt") "needle one\nneedle two\n";
  let output =
    run_tool Tools_search.grep context
      (json_object
         [ ("pattern", Jsont.Json.string "needle"); ("max_results", Jsont.Json.int 1) ])
  in
  Alcotest.(check bool)
    "grep emits the first result" true
    (Test_support.contains ~needle:":1:needle one" ~haystack:output.Tool.content);
  Alcotest.(check bool)
    "grep reports truncation" true
    (Test_support.contains ~needle:"(truncated at 1)" ~haystack:output.Tool.content)

let check_glob_no_match () =
  with_context @@ fun _env _root context ->
  let output =
    run_tool Tools_search.glob context
      (json_object [ ("pattern", Jsont.Json.string "**/*.does-not-exist") ])
  in
  Alcotest.(check string) "glob reports no matches" "No files found" output.Tool.content

let check_registered_tools () =
  let tools = [ Tools_search.ls; Tools_search.glob; Tools_search.grep ] in
  Alcotest.(check (list string))
    "search tool names" [ "ls"; "glob"; "grep" ]
    (List.map (fun (tool : Tool.t) -> tool.Tool.name) tools);
  List.iter
    (fun (tool : Tool.t) ->
      Alcotest.(check bool)
        (tool.Tool.name ^ " is read-only metadata")
        true tool.Tool.read_only;
      match tool.Tool.schema with
      | Jsont.Object _ -> ()
      | _ -> Alcotest.failf "%s schema is not an object" tool.Tool.name)
    tools

let cases =
  [
    Alcotest.test_case "ls skips generated trees" `Quick check_ls_skips_generated_trees;
    Alcotest.test_case "glob and hidden paths" `Quick check_glob_and_direct_hidden_path;
    Alcotest.test_case "grep modes" `Quick check_grep_literal_regex_include_and_binary;
    Alcotest.test_case "grep limit" `Quick check_grep_limit_footer;
    Alcotest.test_case "glob no match" `Quick check_glob_no_match;
    Alcotest.test_case "registered tools" `Quick check_registered_tools;
  ]
