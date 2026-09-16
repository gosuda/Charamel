module Hooks = Crush_core.Hooks
module Config = Crush_core.Config

let json_string value = Jsont.Json.string value

let hook ~event ~command ?(matcher = None) ?(timeout_s = 30) () : Config.hook =
  { event; matcher; command; timeout_s }

let with_environment f =
  Eio_main.run (fun env ->
      let root =
        Filename.concat
          (Filename.get_temp_dir_name ())
          ("crush-hooks-" ^ string_of_int (Unix.getpid ()))
      in
      (try Unix.mkdir root 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      Fun.protect
        (fun () -> f env root)
        ~finally:(fun () ->
          (try
             Array.iter
               (fun name ->
                 let path = Filename.concat root name in
                 try Sys.remove path with Sys_error _ -> ())
               (Sys.readdir root)
           with Sys_error _ -> ());
          try Unix.rmdir root with Unix.Unix_error _ -> ()))

let runner env root config =
  Hooks.create ~config ~proc_mgr:(Eio.Stdenv.process_mgr env)
    ~clock:(Eio.Stdenv.clock env) ~cwd:root

let read_payload env root file = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / file)

let rewritten_input () =
  with_environment (fun env root ->
      let config =
        [
          hook ~event:Config.Pre_tool ~matcher:(Some "read")
            ~command:
              "printf '%s' '{\"decision\":\"allow\",\"input\":{\"path\":\"rewritten\"}}'"
            ();
        ]
      in
      let runner = runner env root config in
      match
        Hooks.pre_tool runner ~session:"s" ~tool:"read" ~input:(json_string "old")
      with
      | Hooks.Allow json ->
          Alcotest.check Alcotest.bool "matcher rewrite" true
            (Jsont.Json.equal json
               (Jsont.Json.object'
                  [ Jsont.Json.mem (Jsont.Json.name "path") (json_string "rewritten") ]))
      | Hooks.Deny reason -> Alcotest.failf "unexpected denial: %s" reason)

let matcher_filters_tools () =
  with_environment (fun env root ->
      let config =
        [
          hook ~event:Config.Pre_tool ~matcher:(Some "write")
            ~command:"printf '%s' '{\"decision\":\"deny\",\"reason\":\"blocked\"}'" ();
        ]
      in
      let runner = runner env root config in
      match
        Hooks.pre_tool runner ~session:"s" ~tool:"read" ~input:(Jsont.Json.null ())
      with
      | Hooks.Allow _ -> ()
      | Hooks.Deny reason -> Alcotest.failf "nonmatching hook denied: %s" reason)

let denied_input () =
  with_environment (fun env root ->
      let config =
        [
          hook ~event:Config.Pre_tool
            ~command:"printf '%s' '{\"decision\":\"deny\",\"reason\":\"no\"}'" ();
        ]
      in
      let runner = runner env root config in
      match
        Hooks.pre_tool runner ~session:"s" ~tool:"write" ~input:(Jsont.Json.null ())
      with
      | Hooks.Deny reason -> Alcotest.check Alcotest.string "denial reason" "no" reason
      | Hooks.Allow _ -> Alcotest.fail "hook denial was ignored")

let exit_two_denies_with_stderr () =
  with_environment (fun env root ->
      let config =
        [ hook ~event:Config.Pre_tool ~command:"printf denied >&2; exit 2" () ]
      in
      let runner = runner env root config in
      match
        Hooks.pre_tool runner ~session:"s" ~tool:"write" ~input:(Jsont.Json.null ())
      with
      | Hooks.Deny reason ->
          Alcotest.check Alcotest.string "exit 2 reason" "denied" reason
      | Hooks.Allow _ -> Alcotest.fail "exit 2 was treated as a pass")

let timeout_denies () =
  with_environment (fun env root ->
      let config = [ hook ~event:Config.Pre_tool ~timeout_s:1 ~command:"sleep 3" () ] in
      let runner = runner env root config in
      match
        Hooks.pre_tool runner ~session:"s" ~tool:"write" ~input:(Jsont.Json.null ())
      with
      | Hooks.Deny reason ->
          Alcotest.check Alcotest.string "timeout reason" "hook sleep 3 timed out" reason
      | Hooks.Allow _ -> Alcotest.fail "timed out hook was treated as a pass")

let post_payload () =
  with_environment (fun env root ->
      let config = [ hook ~event:Config.Post_tool ~command:"cat > post.json" () ] in
      let runner = runner env root config in
      Hooks.post_tool runner ~session:"s" ~tool:"read" ~input:(json_string "in")
        ~output:"out" ~is_error:true;
      let payload = read_payload env root "post.json" in
      Alcotest.check Alcotest.bool "post event" true
        (String.starts_with ~prefix:"{\"event\":\"post_tool\"" payload);
      Alcotest.check Alcotest.bool "post output" true (String.contains payload 'o'))

let stop_payloads () =
  with_environment (fun env root ->
      let config = [ hook ~event:Config.Stop ~command:"cat > stop.json" () ] in
      let runner = runner env root config in
      Hooks.stop runner ~session:"s" ~reason:`Stop;
      let stop = read_payload env root "stop.json" in
      Alcotest.check Alcotest.bool "stop null message" true
        (String.ends_with ~suffix:"\"message\":null}" stop);
      Hooks.stop runner ~session:"s" ~reason:(`Error "boom");
      let error = read_payload env root "stop.json" in
      Alcotest.check Alcotest.bool "stop error message" true
        (String.ends_with ~suffix:"\"message\":\"boom\"}" error))

let lifecycle_payload () =
  with_environment (fun env root ->
      let config = [ hook ~event:Config.Session_start ~command:"cat > hook.json" () ] in
      let runner = runner env root config in
      Hooks.session_start runner ~session:"session-1";
      let payload = read_payload env root "hook.json" in
      let expected =
        "{\"event\":\"session_start\",\"session\":\"session-1\",\"cwd\":\"" ^ root ^ "\"}"
      in
      Alcotest.check Alcotest.string "session payload" expected payload)

let descendant_cleanup () =
  with_environment (fun env root ->
      let config =
        [
          hook ~event:Config.Pre_tool ~timeout_s:1
            ~command:"sleep 30 & echo $! > child.pid; wait" ();
        ]
      in
      let runner = runner env root config in
      (match
         Hooks.pre_tool runner ~session:"s" ~tool:"write" ~input:(Jsont.Json.null ())
       with
      | Hooks.Deny _ -> ()
      | Hooks.Allow _ -> Alcotest.fail "descendant hook did not time out");
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
      let child_path = Filename.concat root "child.pid" in
      if Sys.file_exists child_path then
        let pid =
          int_of_string
            (String.trim (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / child_path)))
        in
        match Unix.kill pid 0 with
        | () -> Alcotest.fail "timed-out hook left a child process"
        | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ())

let cases =
  [
    Alcotest.test_case "pre-tool allow can rewrite input" `Quick rewritten_input;
    Alcotest.test_case "matcher filters tools" `Quick matcher_filters_tools;
    Alcotest.test_case "pre-tool deny stops the call" `Quick denied_input;
    Alcotest.test_case "exit two denies with stderr" `Quick exit_two_denies_with_stderr;
    Alcotest.test_case "timeout is an explicit denial" `Quick timeout_denies;
    Alcotest.test_case "post-tool receives JSON" `Quick post_payload;
    Alcotest.test_case "stop carries null and error messages" `Quick stop_payloads;
    Alcotest.test_case "session-start receives JSON on stdin" `Quick lifecycle_payload;
    Alcotest.test_case "timeout cleans descendant process group" `Quick descendant_cleanup;
  ]
