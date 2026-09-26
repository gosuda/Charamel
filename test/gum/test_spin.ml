open Lwt.Syntax

let stream_order output = String.equal output "outerr" || String.equal output "errout"

let default_env () =
  {
    Charamel_cli.Env.cwd = Unix.getcwd ();
    fs_root = "/";
    stdin = Lwt_io.stdin;
    stdout = Lwt_io.stdout;
    stderr = Lwt_io.stderr;
    clock = Charamel_os.Time.lwt;
  }

let test_child_capture () =
  let* result =
    Spin.run_child (default_env ())
      ~command:[ "sh"; "-c"; "printf out; printf err >&2" ]
      ~timeout:None
  in
  (match result with
  | Error message -> Alcotest.fail message
  | Ok result ->
      Alcotest.(check int) "status" 0 result.Spin.status;
      Alcotest.(check string) "stdout" "out" result.Spin.stdout;
      Alcotest.(check string) "stderr" "err" result.Spin.stderr;
      Alcotest.(check bool) "combined streams" true (stream_order result.Spin.output));
  Lwt.return_unit

let test_child_capture_beyond_memory_buffer () =
  let* result =
    Spin.run_child (default_env ())
      ~command:[ "sh"; "-c"; "head -c 33554433 /dev/zero" ]
      ~timeout:None
  in
  (match result with
  | Error message -> Alcotest.fail message
  | Ok result ->
      Alcotest.(check int) "status" 0 result.Spin.status;
      Alcotest.(check int) "stdout bytes" 33554433 (String.length result.Spin.stdout);
      Alcotest.(check string) "stderr" "" result.Spin.stderr;
      Alcotest.(check bool)
        "combined bytes" true
        (String.equal result.Spin.output result.Spin.stdout));
  Lwt.return_unit

(* The non-interactive command ends by raising [Charamel_cli.exit]'s
   Controlled_exit — the exit-on-success contract. Its constructor is not
   exported, so the expected outcome is identified by its rendered form; the
   status code is pinned, and any other exception propagates and fails the
   case. *)
let exit_is code exn =
  String.starts_with
    ~prefix:(Fmt.str "Charamel_cli.Controlled_exit(%d," code)
    (Printexc.to_string exn)

let await_exit ~code promise =
  Lwt.catch
    (fun () -> promise)
    (fun exn -> if exit_is code exn then Lwt.return_unit else Lwt.fail exn)

let run_non_tty ~code args =
  let diagnostics = Buffer.create 128 in
  let stdin_r, stdin_w = Lwt_io.pipe () in
  let* () = Lwt_io.close stdin_w in
  let stdout_r, stdout_w = Lwt_io.pipe () in
  let stderr_r, stderr_w = Lwt_io.pipe () in
  let stdout_done = Lwt_io.read stdout_r in
  let stderr_done = Lwt_io.read stderr_r in
  let env =
    {
      Charamel_cli.Env.cwd = Unix.getcwd ();
      fs_root = "/";
      stdin = stdin_r;
      stdout = stdout_w;
      stderr = stderr_w;
      clock = Charamel_os.Time.lwt;
    }
  in
  let formatter = Stdlib.Format.formatter_of_buffer diagnostics in
  let* () =
    match
      Cmdliner.Cmd.eval_value ~catch:true ~help:formatter ~err:formatter
        ~env:(fun _ -> None)
        ~argv:(Array.of_list ("spin" :: args))
        (Spin.cmd env)
    with
    | Ok (`Ok promise) -> await_exit ~code promise
    | Ok (`Help | `Version) | Error _ -> Lwt.return_unit
  in
  let* () = Lwt_io.close stdout_w in
  let* () = Lwt_io.close stderr_w in
  let* stdout_text = stdout_done in
  let* stderr_text = stderr_done in
  Stdlib.Format.pp_print_flush formatter ();
  Lwt.return (stdout_text, stderr_text)

let test_show_output_non_tty () =
  let* stdout, stderr =
    run_non_tty ~code:0
      [ "--show-output"; "--"; "sh"; "-c"; "printf out; printf err >&2" ]
  in
  Alcotest.(check bool) "combined output is routed to stdout" true (stream_order stdout);
  Alcotest.(check string) "spinner is not drawn" "Loading...\n" stderr;
  Lwt.return_unit

let test_show_error_non_tty () =
  let* stdout, stderr =
    run_non_tty ~code:7
      [ "--show-error"; "--"; "sh"; "-c"; "printf out; printf err >&2; exit 7" ]
  in
  Alcotest.(check bool)
    "failed combined output is routed to stdout" true (stream_order stdout);
  Alcotest.(check string) "spinner is not drawn" "Loading...\n" stderr;
  Lwt.return_unit

let test_show_error_success_suppresses_output () =
  let* stdout, stderr =
    run_non_tty ~code:0 [ "--show-error"; "--"; "sh"; "-c"; "printf out; printf err >&2" ]
  in
  Alcotest.(check string) "successful output is hidden" "" stdout;
  Alcotest.(check string) "spinner is not drawn" "Loading...\n" stderr;
  Lwt.return_unit

let test_show_stdout_non_tty () =
  let* stdout, _ =
    run_non_tty ~code:0
      [ "--show-stdout"; "--"; "sh"; "-c"; "printf out; printf err >&2" ]
  in
  Alcotest.(check string) "stdout selection" "out" stdout;
  Lwt.return_unit

let test_show_stderr_non_tty () =
  let* stdout, _ =
    run_non_tty ~code:0
      [ "--show-stderr"; "--"; "sh"; "-c"; "printf out; printf err >&2" ]
  in
  Alcotest.(check string) "stderr selection" "err" stdout;
  Lwt.return_unit

let test_child_timeout () =
  let* result =
    Spin.run_child (default_env ()) ~command:[ "sh"; "-c"; "sleep 1" ]
      ~timeout:(Some 0.01)
  in
  (match result with
  | Error message -> Alcotest.fail message
  | Ok result ->
      Alcotest.(check bool) "timeout" true result.Spin.timed_out;
      Alcotest.(check int) "timeout status" 124 result.Spin.status);
  Lwt.return_unit

(* The two-PTY capture path [run_child] gates on the real standard output, which a
   piped [dune runtest] never is, so the ported cases drive [Spin.run_pty_pair]
   directly — the same assertions the Eio-era cases made: a 17x53 child terminal
   reported on both captured streams, and a real stdin pipe reaching the child's
   [read]. Each [stty] queries its own stream's pty. *)
let test_pty_geometry () =
  let* result =
    Spin.run_pty_pair ~rows:17 ~cols:53
      ~command:[ "sh"; "-c"; "stty size <&1; stty size <&2 >&2" ]
      ~timeout:None ()
  in
  (match result with
  | Error message -> Alcotest.fail message
  | Ok result ->
      Alcotest.(check int) "status" 0 result.Spin.status;
      Alcotest.(check string)
        "stdout child terminal size" "17 53"
        (String.trim result.Spin.stdout);
      Alcotest.(check string)
        "stderr child terminal size" "17 53"
        (String.trim result.Spin.stderr));
  Lwt.return_unit

let test_pty_stdin_passthrough () =
  let* result =
    Spin.run_pty_pair ~rows:17 ~cols:53 ~stdin_text:"hello-from-real-stdin\n"
      ~command:[ "sh"; "-c"; "read line; printf 'GOT:%s' \"$line\"" ]
      ~timeout:None ()
  in
  (match result with
  | Error message -> Alcotest.fail message
  | Ok result ->
      Alcotest.(check int) "status" 0 result.Spin.status;
      Alcotest.(check string)
        "stdin passthrough" "GOT:hello-from-real-stdin"
        (String.trim result.Spin.stdout));
  Lwt.return_unit

let cases =
  [
    Alcotest_lwt.test_case "child capture" `Quick (fun _switch () ->
        test_child_capture ());
    Alcotest_lwt.test_case "large child capture" `Quick (fun _switch () ->
        test_child_capture_beyond_memory_buffer ());
    Alcotest_lwt.test_case "show output in pipe mode" `Quick (fun _switch () ->
        test_show_output_non_tty ());
    Alcotest_lwt.test_case "show error in pipe mode" `Quick (fun _switch () ->
        test_show_error_non_tty ());
    Alcotest_lwt.test_case "show error success suppression" `Quick (fun _switch () ->
        test_show_error_success_suppresses_output ());
    Alcotest_lwt.test_case "show stdout in pipe mode" `Quick (fun _switch () ->
        test_show_stdout_non_tty ());
    Alcotest_lwt.test_case "show stderr in pipe mode" `Quick (fun _switch () ->
        test_show_stderr_non_tty ());
    Alcotest_lwt.test_case "PTY geometry" `Quick (fun _switch () -> test_pty_geometry ());
    Alcotest_lwt.test_case "PTY stdin passthrough" `Quick (fun _switch () ->
        test_pty_stdin_passthrough ());
    Alcotest_lwt.test_case "timeout" `Quick (fun _switch () -> test_child_timeout ());
  ]
