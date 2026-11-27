open Lwt_direct
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
module Shell_screen = Crush_core.Shell_screen

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

(* Same shell-compatibility layer as the jobs tests: [set /p] under [<nul]
   reads EOF and reports status 1 despite printing, so the Windows fixtures
   pin the zero exit explicitly. *)
let emit_for ~win32 text =
  if win32 then "(<nul set /p x=" ^ text ^ ")&exit 0" else "printf " ^ text

let emit text = emit_for ~win32:Sys.win32 text

let both_streams_for ~win32 =
  if win32 then "(<nul set /p x=out)&(<nul set /p x=err 1>&2)&exit 0"
  else "printf out; printf err >&2"

let both_streams = both_streams_for ~win32:Sys.win32

let long_sleep seconds =
  if Sys.win32 then "ping -n " ^ string_of_int (seconds + 1) ^ " 127.0.0.1 >nul"
  else "sleep " ^ string_of_int seconds

let make_ctx ?asker sw =
  let open Lwt.Syntax in
  let clock = Charamel_os.Time.lwt in
  let cwd = Filename.get_temp_dir_name () in
  let config = Config.default in
  let permission =
    Permission.create ~config:config.Config.permissions ~yolo:true ?asker ~cwd
      ~plans_dir:(Filename.concat cwd ".crush/plans")
      ()
  in
  let hooks = Hooks.create ~config:[] ~cwd in
  let* mcp = Mcp.create ~cwd ~config in
  let artifacts = Artifact.create ~fs_root:"/" ~dir:"tmp/crush-shell-test-artifacts" in
  let jobs = Jobs.create ~sw ~artifacts in
  let+ skills = Skills.load ~fs_root:"/" ~config ~home:cwd in
  {
    Tool.clock;
    fs_root = "/";
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
    log_path = Filename.concat cwd "crush-shell-test.log";
    interactive = false;
    is_subagent = false;
    ask = None;
    run_subagent = None;
    read_tracker = Hashtbl.create 8;
  }

let with_ctx ?asker f =
  let open Lwt.Syntax in
  Lwt_switch.with_switch (fun sw ->
      let* ctx = make_ctx ?asker sw in
      Lwt_direct.spawn (fun () -> f ctx))

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

let run_command ctx command =
  Tools_shell.bash.Tool.run ctx (bash_value ~command ~description:"screen" ())

(* Each form makes /bin/sh start curl. Yolo approves every ordinary request, so the
   screen alone decides whether the command runs. *)
let refused_forms =
  [
    "c''url --version";
    "cu\"\"rl --version";
    "c\\url --version";
    "$'\\x63url' --version";
    "'cu'\"rl\" --version";
    "/usr/bin/c''url --version";
    "sh -c \"c''url --version\"";
    "echo $(c''url --version)";
  ]

let test_screen_reads_quoting () =
  if not Sys.win32 then
    await @@ with_ctx
    @@ fun ctx ->
    let unrefused =
      List.filter
        (fun command ->
          match run_command ctx command with
          | Error (`Unavailable _) -> false
          | Ok _ | Error _ -> true)
        refused_forms
    in
    Alcotest.(check (list string)) "every quoted form is refused" [] unrefused

(* The shell computes the program each form runs, so no blanket approval may run it. *)
let computed_forms =
  [
    "$(printf ech)o reviewed";
    "X=ech; ${X}o reviewed";
    "`printf ech`o reviewed";
    "printf 'echo reviewed' | sh";
    "env $(printf ech)o reviewed";
    ". /dev/null";
    "printf $'\\'\n$(printf ech)o reviewed\n'";
  ]

let test_screen_requires_review () =
  if not Sys.win32 then (
    await @@ with_ctx
    @@ fun ctx ->
    let unreviewed =
      List.filter
        (fun command ->
          match run_command ctx command with
          | Error (`Denied _) -> false
          | Ok _ | Error _ -> true)
        computed_forms
    in
    Alcotest.(check (list string)) "yolo does not run a computed program" [] unreviewed;
    await
    @@ with_ctx ~asker:(fun _ -> Permission.Allow_once)
    @@ fun ctx ->
    let output = output_or_fail (run_command ctx "$(printf ech)o reviewed") in
    Alcotest.(check bool)
      "an explicit approval runs it" true
      (Test_support.contains ~needle:"reviewed" ~haystack:output.Tool.content))

let ordinary_forms =
  [
    ("cd \"$(dirname /tmp/x)\" && printf ok", "ok");
    ("for word in a b; do printf \"$word\"; done", "ab");
    ("printf '%s' \"$(printf nested)\"", "nested");
    ("sh -c 'printf inner'", "inner");
    ("cat <<'EOF'\nheredoc $(not run)\nEOF", "heredoc $(not run)");
  ]

let test_screen_clears_ordinary () =
  if not Sys.win32 then
    await @@ with_ctx
    @@ fun ctx ->
    List.iter
      (fun (command, expected) ->
        let output = output_or_fail (run_command ctx command) in
        Alcotest.(check bool)
          command true
          (Test_support.contains ~needle:expected ~haystack:output.Tool.content))
      ordinary_forms

let screen_table =
  let posix command expected = (Shell_screen.Posix, command, expected) in
  let cmd command expected = (Shell_screen.Cmd, command, expected) in
  [
    posix "git status --short | head -20" `Clear;
    posix "cd \"$(git rev-parse --show-toplevel)\" && dune build" `Clear;
    posix "for f in *.ml; do ocamlformat \"$f\"; done" `Clear;
    posix "case \"$1\" in a) echo a;; *) echo b;; esac" `Clear;
    posix "FOO=$(date) make test" `Clear;
    posix "sh script.sh" `Clear;
    posix "xargs -I {} cp {} /tmp" `Clear;
    posix "find . -name '*.ml' -exec grep -l x {} +" `Clear;
    posix "curl https://example.com" `Blocked;
    posix "cu\\\nrl x" `Blocked;
    posix "$'\\143url' x" `Blocked;
    posix "$'\\u0063url' x" `Blocked;
    posix "bash -c 'c\"\"url x'" `Blocked;
    posix "timeout 5s sh -c \"c''url x\"" `Blocked;
    posix "watch \"c''url x\"" `Blocked;
    posix "x=$(c''url x)" `Blocked;
    posix "cat <<EOF\n$(c''url x)\nEOF" `Blocked;
    posix "/usr/local/bin/brew install x" `Blocked;
    posix "$CMD x" `Review;
    posix "\"$CMD\" x" `Review;
    posix "c*l x" `Review;
    posix "c{u,}rl x" `Review;
    posix "{fd}>/dev/null $CMD" `Review;
    posix "true &>/dev/null $CMD" `Review;
    posix "(( $CMD ))" `Review;
    posix "[[ -n x || $CMD ]]" `Review;
    posix "printf $'\\'\n$CMD\n'" `Review;
    posix "xargs -I{} {} x" `Review;
    posix "find /usr/bin -name x -exec {} \\;" `Review;
    posix "env -S \"$CMD x\"" `Review;
    posix "fish -c 'echo hi'" `Review;
    posix "echo 'unterminated" `Review;
    posix
      (String.concat "" (List.init 5000 (fun _ -> "$(")) ^ "x" ^ String.make 5000 ')')
      `Review;
    posix "script -qc \"c''url\" /dev/null" `Blocked;
    posix "echo hi | xargs -I % % x" `Review;
    posix "cat <<EOF | sh\necho hi\nEOF" `Review;
    cmd "for %f in (a.txt b.txt) do type \"%f\"" `Clear;
    cmd "for %%f in (a.txt b.txt) do type \"%%f\"" `Clear;
    cmd "for /l %i in (1,1,10) do @echo hi" `Clear;
    cmd "for %f in (a.txt b.txt)" `Review;
    cmd "for %f in (%FILES%) do type \"%f\"" `Review;
    cmd "for /f \"tokens=*\" %i in ('dir') do echo %i" `Review;
    cmd "c^url x" `Blocked;
    cmd "cu\"\"rl x" `Blocked;
    cmd "C:\\Windows\\System32\\curl.exe x" `Blocked;
    cmd "cmd /c \"c^url x\"" `Blocked;
    cmd "%CMD% x" `Review;
  ]

let test_screen_table () =
  let kind = function
    | Shell_screen.Clear -> `Clear
    | Shell_screen.Blocked _ -> `Blocked
    | Shell_screen.Review _ -> `Review
  in
  let mismatched =
    List.filter_map
      (fun (dialect, command, expected) ->
        if kind (Shell_screen.screen dialect command) = expected then None
        else Some command)
      screen_table
  in
  Alcotest.(check (list string)) "every command gets its expected verdict" [] mismatched

let test_foreground_capture () =
  await @@ with_ctx
  @@ fun ctx ->
  let input = bash_value ~command:both_streams ~description:"capture both streams" () in
  let output = output_or_fail (Tools_shell.bash.Tool.run ctx input) in
  Alcotest.(check bool)
    "stdout is captured" true
    (Test_support.contains ~needle:"out" ~haystack:output.Tool.content);
  Alcotest.(check bool)
    "stderr is captured" true
    (Test_support.contains ~needle:"err" ~haystack:output.Tool.content);
  Alcotest.(check bool) "successful process is not an error" false output.Tool.is_error

let test_timeout () =
  await @@ with_ctx
  @@ fun ctx ->
  let input =
    bash_value ~timeout_s:1 ~command:(long_sleep 5) ~description:"deadline" ()
  in
  match Tools_shell.bash.Tool.run ctx input with
  | Error (`Timeout seconds) -> Alcotest.(check (float 1e-9)) "deadline" 1. seconds
  | Error error -> Alcotest.failf "unexpected timeout result: %a" Tool.pp_error error
  | Ok _ -> Alcotest.fail "sleep exceeded its deadline"

let test_background_lifecycle () =
  await @@ with_ctx
  @@ fun ctx ->
  let started =
    output_or_fail
      (Tools_shell.bash.Tool.run ctx
         (bash_value ~timeout_s:5 ~run_in_background:true ~command:(emit "background")
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
         (bash_value ~timeout_s:5 ~run_in_background:true ~command:(long_sleep 30)
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

(* Linux cannot execute the [cmd /c] fixtures, so this pins their shape: the
   Windows fixtures must end with an explicit zero exit. *)
let windows_fixtures_pin_zero_exit () =
  Alcotest.(check bool)
    "windows emit ends with an explicit zero exit" true
    (String.ends_with ~suffix:"&exit 0" (emit_for ~win32:true "probe"));
  Alcotest.(check bool)
    "windows both_streams ends with an explicit zero exit" true
    (String.ends_with ~suffix:"&exit 0" (both_streams_for ~win32:true))

let cases =
  [
    Test_tools_test_support.case "conservative shell classifier" `Quick test_classifier;
    Test_tools_test_support.case "screen reads shell quoting" `Quick
      test_screen_reads_quoting;
    Test_tools_test_support.case "screen requires review" `Quick
      test_screen_requires_review;
    Test_tools_test_support.case "screen clears ordinary commands" `Quick
      test_screen_clears_ordinary;
    Test_tools_test_support.case "screen verdict table" `Quick test_screen_table;
    Test_tools_test_support.case "foreground capture" `Quick test_foreground_capture;
    Test_tools_test_support.case "foreground deadline" `Quick test_timeout;
    Test_tools_test_support.case "background lifecycle" `Quick test_background_lifecycle;
    Test_tools_test_support.case "windows fixtures pin zero exit" `Quick
      windows_fixtures_pin_zero_exit;
  ]
