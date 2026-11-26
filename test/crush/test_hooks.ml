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

(* The stop and session-start payloads are compared as the JSON they encode: the
   Windows [capture] is [findstr], which ends what it copies with CRLF, and a
   Windows [cwd] reaches the hook with every backslash JSON-escaped. *)
let read_json root file =
  let text = read_payload root file in
  match Jsont_bytesrw.decode_string Jsont.json text with
  | Ok json -> json
  | Error message -> Alcotest.failf "%s holds no JSON value (%s): %S" file message text

let failure_reason = "pre-tool hook failed"

let exit_one =
  if Sys.win32 then "echo secret 1>&2 & exit /b 1" else "printf secret >&2; exit 1"

let json = Alcotest.testable Jsont.pp_json Jsont.Json.equal

let member name = function
  | Jsont.Object (members, _) -> Option.map snd (Jsont.Json.find_mem name members)
  | _ -> None

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

(* An over-limit stderr is a warning on a successful exit, not a denial: the hook prints
   valid allow JSON on stdout and more than [max_output_size] bytes on stderr. *)
let oversized_stderr_allows () =
  if Sys.win32 then
    (* cmd has no [head] or [tr]; the output cap is the same code path on Windows. *)
    Alcotest.skip ()
  else
    with_environment (fun root ->
        let command =
          "printf '{\"decision\":\"allow\"}'; head -c 1048577 /dev/zero | tr '\\000' ' ' \
           >&2"
        in
        let input = json_string "kept" in
        match
          pre_tool_outcome (runner root [ hook ~event:Config.Pre_tool ~command () ]) input
        with
        | Hooks.Allow json ->
            Alcotest.check Alcotest.bool "input kept" true (Jsont.Json.equal json input)
        | Hooks.Deny reason ->
            Alcotest.failf "oversized stderr denied an allowed call: %s" reason)

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
      let stop = read_json root "stop.json" in
      Alcotest.check (Alcotest.option json) "stop null message"
        (Some (Jsont.Json.null ()))
        (member "message" stop);
      await (Hooks.stop runner ~session:"s" ~reason:(`Error "boom"));
      let error = read_json root "stop.json" in
      Alcotest.check (Alcotest.option json) "stop error message"
        (Some (json_string "boom"))
        (member "message" error))

let lifecycle_payload () =
  with_environment (fun root ->
      let config =
        [ hook ~event:Config.Session_start ~command:(capture "hook.json") () ]
      in
      let runner = runner root config in
      await (Hooks.session_start runner ~session:"session-1");
      let expected =
        Jsont.Json.object'
          [
            Jsont.Json.mem (Jsont.Json.name "event") (json_string "session_start");
            Jsont.Json.mem (Jsont.Json.name "session") (json_string "session-1");
            Jsont.Json.mem (Jsont.Json.name "cwd") (json_string root);
          ]
      in
      Alcotest.check json "session payload" expected (read_json root "hook.json"))

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

let successful_hook_keeps_detached_worker () =
  if Sys.win32 then Alcotest.skip ()
  else
    with_environment (fun root ->
        let release_path = Filename.concat root "worker-release" in
        let finished_path = Filename.concat root "worker-finished" in
        Unix.mkfifo release_path 0o600;
        Unix.mkfifo finished_path 0o600;
        let open_fifo path =
          Unix.openfile path [ Unix.O_RDWR; Unix.O_NONBLOCK ] 0
          |> Lwt_unix.of_unix_file_descr
        in
        let release = Lwt_io.of_fd ~mode:Lwt_io.output (open_fifo release_path) in
        let finished = Lwt_io.of_fd ~mode:Lwt_io.input (open_fifo finished_path) in
        let pid_path = Filename.concat root "worker.pid" in
        let completed = ref false in
        Fun.protect
          ~finally:(fun () ->
            (if (not !completed) && Sys.file_exists pid_path then
               let pid = int_of_string (String.trim (read_payload root "worker.pid")) in
               try Unix.kill pid Sys.sigkill
               with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
            await (Lwt_io.close release);
            await (Lwt_io.close finished))
          (fun () ->
            let command =
              "cat >/dev/null; (read release < worker-release; printf 'done\\n' > \
               worker-finished) </dev/null >/dev/null 2>&1 & printf '%s\\n' \"$!\" > \
               worker.pid"
            in
            let runner = runner root [ hook ~event:Config.Post_tool ~command () ] in
            await
            @@ Hooks.post_tool runner ~session:"s" ~tool:"write"
                 ~input:(Jsont.Json.null ()) ~output:"done" ~is_error:false;
            await (Lwt_io.write_line release "continue");
            await (Lwt_io.flush release);
            let message =
              await @@ Lwt_unix.with_timeout 5. (fun () -> Lwt_io.read_line finished)
            in
            completed := true;
            Alcotest.(check string)
              "detached worker finishes after hook returns" "done" message))

let failing_hook_kills_detached_child () =
  if Sys.win32 then
    (* cmd cannot publish a child's pid for [kill]; the tree-kill coverage lives
       in the jobs tests instead. *)
    Alcotest.skip ()
  else
    with_environment (fun root ->
        let config =
          [
            hook ~event:Config.Pre_tool
              ~command:"sleep 30 >/dev/null 2>&1 & echo $! > child.pid; exit 2" ();
          ]
        in
        let runner = runner root config in
        (match
           await
           @@ Hooks.pre_tool runner ~session:"s" ~tool:"write" ~input:(Jsont.Json.null ())
         with
        | Hooks.Deny _ -> ()
        | Hooks.Allow _ -> Alcotest.fail "failing hook was treated as a pass");
        (* An orphaned zombie still answers signal 0 until it is reaped. Wait
           past that window so the check below proves a live child. *)
        await (Lwt_unix.sleep 0.5);
        let child_path = Filename.concat root "child.pid" in
        if not (Sys.file_exists child_path) then
          Alcotest.fail "hook exited before recording its child";
        let pid =
          int_of_string (String.trim (Test_tools_test_support.load_file child_path))
        in
        match Unix.kill pid 0 with
        | () -> Alcotest.fail "failing hook left its detached child"
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
    Test_tools_test_support.case "oversized stderr on success still allows" `Quick
      oversized_stderr_allows;
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
    Test_tools_test_support.case "successful hook preserves detached worker" `Quick
      successful_hook_keeps_detached_worker;
    Test_tools_test_support.case "failing hook kills detached child" `Quick
      failing_hook_kills_detached_child;
  ]
