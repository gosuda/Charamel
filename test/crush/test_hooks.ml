module Hooks = Crush_core.Hooks
module Config = Crush_core.Config
open Lwt_direct

let json_string value = Jsont.Json.string value

let hook ~event ~command ?(matcher = None) ?(timeout_s = 30) () : Config.hook =
  { event; matcher; command; timeout_s }

(* The hook commands run under the platform shell: [echo] prints a payload on
   both shells, [ping -n] stands in for [sleep] (cmd's [timeout] refuses
   redirected stdin), and [findstr] is cmd's [cat]. *)
let emit payload =
  if Sys.win32 then "echo " ^ payload else "printf '%s' '" ^ payload ^ "'"

let exit_two =
  if Sys.win32 then "echo denied 1>&2 & exit /b 2" else "printf denied >&2; exit 2"

let long_sleep seconds =
  if Sys.win32 then "ping -n " ^ string_of_int (seconds + 1) ^ " 127.0.0.1 >nul"
  else "sleep " ^ string_of_int seconds

let capture file = if Sys.win32 then "findstr \"^\" > " ^ file else "cat > " ^ file
let with_environment f = Test_tools_test_support.with_scratch (fun root -> f root)
let runner root config = Hooks.create ~config ~cwd:root
let read_payload root file = Test_tools_test_support.load_file (Filename.concat root file)
let failure_reason = "pre-tool hook failed"

let exit_one =
  if Sys.win32 then "echo secret 1>&2 & exit /b 1" else "printf secret >&2; exit 1"

let oversized_deny =
  "printf '%s' '{\"decision\":\"deny\"}'; head -c 1048576 /dev/zero | tr '\\000' ' '"

let pre_tool_outcome runner input =
  await @@ Hooks.pre_tool runner ~session:"s" ~tool:"write" ~input

let expect_failure_denial label runner =
  match pre_tool_outcome runner (Jsont.Json.null ()) with
  | Hooks.Deny reason -> Alcotest.check Alcotest.string label failure_reason reason
  | Hooks.Allow _ -> Alcotest.failf "%s: an unevaluable hook allowed the call" label

let rewritten_input () =
  with_environment (fun root ->
      let config =
        [
          hook ~event:Config.Pre_tool ~matcher:(Some "read")
            ~command:(emit "{\"decision\":\"allow\",\"input\":{\"path\":\"rewritten\"}}")
            ();
        ]
      in
      let runner = runner root config in
      match
        await
        @@ Hooks.pre_tool runner ~session:"s" ~tool:"read" ~input:(json_string "old")
      with
      | Hooks.Allow json ->
          Alcotest.check Alcotest.bool "matcher rewrite" true
            (Jsont.Json.equal json
               (Jsont.Json.object'
                  [ Jsont.Json.mem (Jsont.Json.name "path") (json_string "rewritten") ]))
      | Hooks.Deny reason -> Alcotest.failf "unexpected denial: %s" reason)

let matcher_filters_tools () =
  with_environment (fun root ->
      let config =
        [
          hook ~event:Config.Pre_tool ~matcher:(Some "write")
            ~command:(emit "{\"decision\":\"deny\",\"reason\":\"blocked\"}")
            ();
        ]
      in
      let runner = runner root config in
      match
        await
        @@ Hooks.pre_tool runner ~session:"s" ~tool:"read" ~input:(Jsont.Json.null ())
      with
      | Hooks.Allow _ -> ()
      | Hooks.Deny reason -> Alcotest.failf "nonmatching hook denied: %s" reason)

let denied_input () =
  with_environment (fun root ->
      let config =
        [
          hook ~event:Config.Pre_tool
            ~command:(emit "{\"decision\":\"deny\",\"reason\":\"no\"}")
            ();
        ]
      in
      let runner = runner root config in
      match
        await
        @@ Hooks.pre_tool runner ~session:"s" ~tool:"write" ~input:(Jsont.Json.null ())
      with
      | Hooks.Deny reason -> Alcotest.check Alcotest.string "denial reason" "no" reason
      | Hooks.Allow _ -> Alcotest.fail "hook denial was ignored")

let exit_two_denies_with_stderr () =
  with_environment (fun root ->
      let config = [ hook ~event:Config.Pre_tool ~command:exit_two () ] in
      let runner = runner root config in
      match
        await
        @@ Hooks.pre_tool runner ~session:"s" ~tool:"write" ~input:(Jsont.Json.null ())
      with
      | Hooks.Deny reason ->
          Alcotest.check Alcotest.string "exit 2 reason" "denied" reason
      | Hooks.Allow _ -> Alcotest.fail "exit 2 was treated as a pass")

let timeout_denies () =
  with_environment (fun root ->
      let config =
        [ hook ~event:Config.Pre_tool ~timeout_s:1 ~command:(long_sleep 3) () ]
      in
      let runner = runner root config in
      match
        await
        @@ Hooks.pre_tool runner ~session:"s" ~tool:"write" ~input:(Jsont.Json.null ())
      with
      | Hooks.Deny reason ->
          Alcotest.check Alcotest.string "timeout reason"
            ("hook " ^ long_sleep 3 ^ " timed out")
            reason
      | Hooks.Allow _ -> Alcotest.fail "timed out hook was treated as a pass")

let failing_exit_denies () =
  with_environment (fun root ->
      expect_failure_denial "exit 1"
        (runner root [ hook ~event:Config.Pre_tool ~command:exit_one () ]))

let spawn_failure_denies () =
  with_environment (fun root ->
      let config =
        [ hook ~event:Config.Pre_tool ~command:(emit "{\"decision\":\"allow\"}") () ]
      in
      expect_failure_denial "spawn failure"
        (Hooks.create ~config ~cwd:(Filename.concat root "missing")))

let malformed_output_denies () =
  with_environment (fun root ->
      let allowed =
        List.filter
          (fun output ->
            let config = [ hook ~event:Config.Pre_tool ~command:(emit output) () ] in
            match pre_tool_outcome (runner root config) (Jsont.Json.null ()) with
            | Hooks.Deny reason ->
                Alcotest.check Alcotest.string output failure_reason reason;
                false
            | Hooks.Allow _ -> true)
          [ "not json"; "{\"decision\":"; "[\"allow\"]"; "{\"decision\":\"maybe\"}" ]
      in
      Alcotest.check Alcotest.(list string) "outputs that allowed the call" [] allowed)

let oversized_output_denies () =
  if Sys.win32 then
    (* cmd has no [head] or [tr]; the output cap is the same code path on Windows. *)
    Alcotest.skip ()
  else
    with_environment (fun root ->
        expect_failure_denial "oversized output"
          (runner root [ hook ~event:Config.Pre_tool ~command:oversized_deny () ]))

let well_formed_allow_keeps_input () =
  with_environment (fun root ->
      let input = json_string "kept" in
      List.iter
        (fun command ->
          match
            pre_tool_outcome
              (runner root [ hook ~event:Config.Pre_tool ~command () ])
              input
          with
          | Hooks.Allow json ->
              Alcotest.check Alcotest.bool command true (Jsont.Json.equal json input)
          | Hooks.Deny reason ->
              Alcotest.failf "%s: well-formed allow denied: %s" command reason)
        [ "exit 0"; emit "{\"decision\":\"allow\"}" ])

let lifecycle_failures_stay_observational () =
  with_environment (fun root ->
      let config =
        List.map
          (fun event -> hook ~event ~command:exit_one ())
          [ Config.Post_tool; Config.Session_start; Config.Stop ]
      in
      List.iter
        (fun hooks ->
          await
          @@ Hooks.post_tool hooks ~session:"s" ~tool:"read" ~input:(json_string "in")
               ~output:"out" ~is_error:false;
          await (Hooks.session_start hooks ~session:"s");
          await (Hooks.stop hooks ~session:"s" ~reason:`Stop))
        [ runner root config; Hooks.create ~config ~cwd:(Filename.concat root "missing") ])

let post_payload () =
  with_environment (fun root ->
      let config = [ hook ~event:Config.Post_tool ~command:(capture "post.json") () ] in
      let runner = runner root config in
      await
      @@ Hooks.post_tool runner ~session:"s" ~tool:"read" ~input:(json_string "in")
           ~output:"out" ~is_error:true;
      let payload = read_payload root "post.json" in
      Alcotest.check Alcotest.bool "post event" true
        (String.starts_with ~prefix:"{\"event\":\"post_tool\"" payload);
      Alcotest.check Alcotest.bool "post output" true (String.contains payload 'o'))

let stop_payloads () =
  with_environment (fun root ->
      let config = [ hook ~event:Config.Stop ~command:(capture "stop.json") () ] in
      let runner = runner root config in
      await (Hooks.stop runner ~session:"s" ~reason:`Stop);
      let stop = read_payload root "stop.json" in
      Alcotest.check Alcotest.bool "stop null message" true
        (String.ends_with ~suffix:"\"message\":null}" stop);
      await (Hooks.stop runner ~session:"s" ~reason:(`Error "boom"));
      let error = read_payload root "stop.json" in
      Alcotest.check Alcotest.bool "stop error message" true
        (String.ends_with ~suffix:"\"message\":\"boom\"}" error))

let lifecycle_payload () =
  with_environment (fun root ->
      let config =
        [ hook ~event:Config.Session_start ~command:(capture "hook.json") () ]
      in
      let runner = runner root config in
      await (Hooks.session_start runner ~session:"session-1");
      let payload = read_payload root "hook.json" in
      let expected =
        "{\"event\":\"session_start\",\"session\":\"session-1\",\"cwd\":\"" ^ root ^ "\"}"
      in
      Alcotest.check Alcotest.string "session payload" expected payload)

let descendant_cleanup () =
  if Sys.win32 then
    (* cmd cannot publish a child's pid for [kill]; the tree-kill coverage lives
       in the jobs tests instead. *)
    Alcotest.skip ()
  else
    with_environment (fun root ->
        let config =
          [
            hook ~event:Config.Pre_tool ~timeout_s:1
              ~command:"sleep 30 & echo $! > child.pid; wait" ();
          ]
        in
        let runner = runner root config in
        (match
           await
           @@ Hooks.pre_tool runner ~session:"s" ~tool:"write" ~input:(Jsont.Json.null ())
         with
        | Hooks.Deny _ -> ()
        | Hooks.Allow _ -> Alcotest.fail "descendant hook did not time out");
        await (Lwt_unix.sleep 0.1);
        let child_path = Filename.concat root "child.pid" in
        if Sys.file_exists child_path then
          let pid =
            int_of_string (String.trim (Test_tools_test_support.load_file child_path))
          in
          match Unix.kill pid 0 with
          | () -> Alcotest.fail "timed-out hook left a child process"
          | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ())

let cases =
  [
    Test_tools_test_support.case "pre-tool allow can rewrite input" `Quick rewritten_input;
    Test_tools_test_support.case "matcher filters tools" `Quick matcher_filters_tools;
    Test_tools_test_support.case "pre-tool deny stops the call" `Quick denied_input;
    Test_tools_test_support.case "exit two denies with stderr" `Quick
      exit_two_denies_with_stderr;
    Test_tools_test_support.case "timeout is an explicit denial" `Quick timeout_denies;
    Test_tools_test_support.case "failing pre-tool hook denies" `Quick failing_exit_denies;
    Test_tools_test_support.case "unstartable pre-tool hook denies" `Quick
      spawn_failure_denies;
    Test_tools_test_support.case "malformed pre-tool output denies" `Quick
      malformed_output_denies;
    Test_tools_test_support.case "oversized pre-tool output denies" `Quick
      oversized_output_denies;
    Test_tools_test_support.case "well-formed allow keeps the input" `Quick
      well_formed_allow_keeps_input;
    Test_tools_test_support.case "failing lifecycle hooks stay observational" `Quick
      lifecycle_failures_stay_observational;
    Test_tools_test_support.case "post-tool receives JSON" `Quick post_payload;
    Test_tools_test_support.case "stop carries null and error messages" `Quick
      stop_payloads;
    Test_tools_test_support.case "session-start receives JSON on stdin" `Quick
      lifecycle_payload;
    Test_tools_test_support.case "timeout cleans descendant process group" `Quick
      descendant_cleanup;
  ]
