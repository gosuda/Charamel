module Artifact = Crush_core.Artifact
module Config = Crush_core.Config
module Hooks = Crush_core.Hooks
module Jobs = Crush_core.Jobs
module Mcp = Crush_core.Mcp
module Permission = Crush_core.Permission
module Skills = Crush_core.Skills
module Todos = Crush_core.Todos
module Tool = Crush_core.Tool
module Tools_shell = Crush_core.Tools_shell

let json_field name value = Jsont.Json.mem (Jsont.Json.name name) value

let bash_value ?working_dir ?timeout_s ?(run_in_background = false) ~command ~description
    () =
  let optional name value fields =
    match value with None -> fields | Some value -> json_field name value :: fields
  in
  [
    json_field "command" (Jsont.Json.string command);
    json_field "description" (Jsont.Json.string description);
    json_field "run_in_background" (Jsont.Json.bool run_in_background);
  ]
  |> optional "working_dir" (Option.map Jsont.Json.string working_dir)
  |> optional "timeout_s" (Option.map Jsont.Json.int timeout_s)
  |> Jsont.Json.object'

let job_value id =
  Jsont.Json.object'
    [
      json_field "job_id" (Jsont.Json.string id); json_field "wait" (Jsont.Json.bool true);
    ]

let job_kill_value id = Jsont.Json.object' [ json_field "job_id" (Jsont.Json.string id) ]

let make_ctx env sw =
  let cwd = "/tmp" in
  let config = Config.default in
  let permission =
    Permission.create ~config:config.Config.permissions ~yolo:true ~cwd
      ~plans_dir:"/tmp/.crush/plans" ()
  in
  let hooks = Hooks.create ~config:[] ~proc_mgr:env#process_mgr ~clock:env#clock ~cwd in
  let mcp =
    Mcp.create ~sw ~proc_mgr:env#process_mgr ~net:env#net ~clock:env#clock ~cwd ~config
  in
  let artifacts = Artifact.create ~fs:env#fs ~dir:"/tmp/crush-shell-test-artifacts" in
  let jobs = Jobs.create ~sw ~proc_mgr:env#process_mgr ~clock:env#clock ~artifacts in
  let skills = Skills.load ~fs:env#fs ~config ~home:"/tmp" in
  {
    Tool.sw;
    clock = env#clock;
    fs = env#fs;
    net = env#net;
    proc_mgr = env#process_mgr;
    random = (fun length -> String.make length '\000');
    env = Sys.getenv_opt;
    cwd;
    session = "shell-test";
    call_id = "shell-call";
    config;
    permission;
    hooks;
    lsp = None;
    mcp;
    artifacts;
    jobs;
    todos = Todos.create ();
    skills;
    log_path = "/tmp/crush-shell-test.log";
    interactive = false;
    is_subagent = false;
    ask = None;
    run_subagent = None;
    read_tracker = Hashtbl.create 8;
  }

let with_ctx f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f env (make_ctx env sw)

let output_or_fail = function
  | Ok output -> output
  | Error error -> Alcotest.failf "tool failed: %a" Tool.pp_error error

let test_classifier () =
  Alcotest.(check bool)
    "git status pipeline is read-only" true
    (Tools_shell.is_read_only "git status --short | head -20");
  Alcotest.(check bool)
    "find exec is mutable" false
    (Tools_shell.is_read_only "find . -exec cat {} \\;");
  Alcotest.(check bool)
    "sed in-place is mutable" false
    (Tools_shell.is_read_only "sed -i s/a/b/ file");
  Alcotest.(check bool)
    "awk system form is mutable" false
    (Tools_shell.is_read_only "awk '{ system(\"id\") }' file");
  Alcotest.(check bool)
    "environment assignment is mutable" false
    (Tools_shell.is_read_only "FLAG=value ls")

let test_foreground_capture () =
  with_ctx @@ fun _env ctx ->
  let input =
    bash_value ~command:"printf out; printf err >&2" ~description:"capture both streams"
      ()
  in
  let output = output_or_fail (Tools_shell.bash.Tool.run ctx input) in
  Alcotest.(check bool)
    "stdout is captured" true
    (Test_support.contains ~needle:"out" ~haystack:output.Tool.content);
  Alcotest.(check bool)
    "stderr is captured" true
    (Test_support.contains ~needle:"err" ~haystack:output.Tool.content);
  Alcotest.(check bool) "successful process is not an error" false output.Tool.is_error

let test_timeout () =
  with_ctx @@ fun _env ctx ->
  let input = bash_value ~timeout_s:1 ~command:"sleep 5" ~description:"deadline" () in
  match Tools_shell.bash.Tool.run ctx input with
  | Error (`Timeout seconds) -> Alcotest.(check (float 1e-9)) "deadline" 1. seconds
  | Error error -> Alcotest.failf "unexpected timeout result: %a" Tool.pp_error error
  | Ok _ -> Alcotest.fail "sleep exceeded its deadline"

let test_background_lifecycle () =
  with_ctx @@ fun _env ctx ->
  let started =
    output_or_fail
      (Tools_shell.bash.Tool.run ctx
         (bash_value ~timeout_s:5 ~run_in_background:true ~command:"printf background"
            ~description:"background output" ()))
  in
  let prefix = "started " in
  Alcotest.(check bool)
    "job id is returned" true
    (String.starts_with ~prefix started.Tool.content);
  let id =
    String.sub started.Tool.content (String.length prefix)
      (String.length started.Tool.content - String.length prefix)
  in
  let output = output_or_fail (Tools_shell.job_output.Tool.run ctx (job_value id)) in
  Alcotest.(check bool)
    "background stdout is retained" true
    (Test_support.contains ~needle:"background" ~haystack:output.Tool.content);
  let killed_started =
    output_or_fail
      (Tools_shell.bash.Tool.run ctx
         (bash_value ~timeout_s:5 ~run_in_background:true ~command:"sleep 30"
            ~description:"background kill" ()))
  in
  let killed_id =
    String.sub killed_started.Tool.content (String.length prefix)
      (String.length killed_started.Tool.content - String.length prefix)
  in
  ignore (output_or_fail (Tools_shell.job_kill.Tool.run ctx (job_kill_value killed_id)));
  let killed_output =
    output_or_fail (Tools_shell.job_output.Tool.run ctx (job_value killed_id))
  in
  Alcotest.(check bool)
    "job kill reports killed" true
    (Test_support.contains ~needle:"killed" ~haystack:killed_output.Tool.content)

let cases =
  [
    Alcotest.test_case "conservative shell classifier" `Quick test_classifier;
    Alcotest.test_case "foreground capture" `Quick test_foreground_capture;
    Alcotest.test_case "foreground deadline" `Quick test_timeout;
    Alcotest.test_case "background lifecycle" `Quick test_background_lifecycle;
  ]
