module Tool = Crush_core.Tool
module Config = Crush_core.Config
module Permission = Crush_core.Permission
module Hooks = Crush_core.Hooks
module Artifact = Crush_core.Artifact
module Jobs = Crush_core.Jobs
module Todos = Crush_core.Todos
module Skills = Crush_core.Skills
module Mcp = Crush_core.Mcp
module Tools_search = Crush_core.Tools_search

let contains needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop index =
    if index + needle_length > haystack_length then false
    else if String.sub haystack index needle_length = needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let temp_root () =
  let path = Filename.temp_file "crush-tools-search" ".dir" in
  Sys.remove path;
  path

let make_context env sw root =
  let config = Config.default in
  let permissions : Config.permissions =
    { allowed_tools = [ "ls"; "glob"; "grep" ]; deny = [] }
  in
  let permission =
    Permission.create ~config:permissions ~yolo:false
      ~asker:(fun _ -> Permission.Allow_once)
      ~cwd:root
      ~plans_dir:(Filename.concat root ".crush/plans")
      ()
  in
  let hooks =
    Hooks.create ~config:[] ~proc_mgr:env#process_mgr ~clock:env#clock ~cwd:root
  in
  let artifacts = Artifact.create ~fs:env#fs ~dir:(Filename.concat root ".artifacts") in
  let jobs = Jobs.create ~sw ~proc_mgr:env#process_mgr ~clock:env#clock ~artifacts in
  let mcp =
    Mcp.create ~sw ~proc_mgr:env#process_mgr ~net:env#net ~clock:env#clock ~cwd:root
      ~config
  in
  let todos = Todos.create () in
  let skills = Skills.load ~fs:env#fs ~config ~home:root in
  {
    Tool.sw;
    clock = env#clock;
    fs = env#fs;
    net = env#net;
    proc_mgr = env#process_mgr;
    random = (fun length -> String.make length '\001');
    env = Sys.getenv_opt;
    cwd = root;
    session = "test-session";
    call_id = "test-call";
    config;
    permission;
    hooks;
    lsp = None;
    mcp;
    artifacts;
    jobs;
    todos;
    skills;
    log_path = Filename.concat root "crush.log";
    interactive = false;
    is_subagent = false;
    ask = None;
    run_subagent = None;
    read_tracker = Hashtbl.create 16;
  }

let with_context f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let root = temp_root () in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(env#fs / root);
  let context = make_context env sw root in
  f env root context

let write_file env path content =
  let target = Eio.Path.(env#fs / path) in
  begin match Eio.Path.split target with
  | Some (parent, _) -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent
  | None -> ()
  end;
  Eio.Path.save ~create:(`Or_truncate 0o644) target content

let run_tool tool context value =
  match tool.Tool.run context value with
  | Ok output -> output
  | Error error -> Alcotest.failf "%a" Tool.pp_error error

let json_object fields =
  Jsont.Json.object'
    (List.map (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value) fields)

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
    (contains "a.ml" output.Tool.content);
  Alcotest.(check bool)
    "listing skips hidden entries" false
    (contains ".env" output.Tool.content);
  Alcotest.(check bool)
    "listing skips .git" false
    (contains "ignored.ml" output.Tool.content);
  Alcotest.(check bool)
    "listing skips build output" false
    (contains "generated.ml" output.Tool.content);
  Alcotest.(check bool)
    "listing skips node_modules" false
    (contains "dependency.ml" output.Tool.content)

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
    (contains "src/a.ml" output.Tool.content);
  Alcotest.(check bool)
    "glob finds all source files" true
    (contains "src/b.ml" output.Tool.content);
  Alcotest.(check bool)
    "glob skips generated hidden trees" false
    (contains "ignored.ml" output.Tool.content);
  let direct =
    run_tool Tools_search.glob context
      (json_object [ ("pattern", Jsont.Json.string ".env") ])
  in
  Alcotest.(check bool)
    "explicit hidden glob finds the entry" true
    (contains ".env" direct.Tool.content)

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
    (contains "src/a.ml:1:let needle = 1" literal.Tool.content);
  Alcotest.(check bool)
    "include filters nonmatching basenames" false
    (contains "src/b.txt" literal.Tool.content);
  Alcotest.(check bool)
    "grep skips binary files" false
    (contains "binary.bin" literal.Tool.content);
  Alcotest.(check bool)
    "grep skips hidden trees" false
    (contains "ignored.ml" literal.Tool.content);
  let regex =
    run_tool Tools_search.grep context
      (json_object [ ("pattern", Jsont.Json.string "needle.*text") ])
  in
  Alcotest.(check bool)
    "regex grep matches text" true
    (contains "src/b.txt:1" regex.Tool.content)

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
    (contains ":1:needle one" output.Tool.content);
  Alcotest.(check bool)
    "grep reports truncation" true
    (contains "(truncated at 1)" output.Tool.content)

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
