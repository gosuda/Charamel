let stream_order output = String.equal output "outerr" || String.equal output "errout"

let test_child_capture () =
  Eio_main.run @@ fun env ->
  match
    Spin.run_child env ~command:[ "sh"; "-c"; "printf out; printf err >&2" ] ~timeout:None
  with
  | Error message -> Alcotest.fail message
  | Ok result ->
      Alcotest.(check int) "status" 0 result.Spin.status;
      Alcotest.(check string) "stdout" "out" result.Spin.stdout;
      Alcotest.(check string) "stderr" "err" result.Spin.stderr;
      Alcotest.(check bool) "combined streams" true (stream_order result.Spin.output)

let test_child_capture_beyond_memory_buffer () =
  Eio_main.run @@ fun env ->
  match
    Spin.run_child env ~command:[ "sh"; "-c"; "head -c 33554433 /dev/zero" ] ~timeout:None
  with
  | Error message -> Alcotest.fail message
  | Ok result ->
      Alcotest.(check int) "status" 0 result.Spin.status;
      Alcotest.(check int) "stdout bytes" 33554433 (String.length result.Spin.stdout);
      Alcotest.(check string) "stderr" "" result.Spin.stderr;
      Alcotest.(check bool)
        "combined bytes" true
        (String.equal result.Spin.output result.Spin.stdout)

let run_non_tty args =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdout = Buffer.create 128 in
  let stderr = Buffer.create 128 in
  let diagnostics = Buffer.create 128 in
  let (stdin : Eio_unix.source_ty Eio.Resource.t), stdin_sink = Eio_unix.pipe sw in
  Eio.Resource.close stdin_sink;
  let (stdout_source : Eio_unix.source_ty Eio.Resource.t), stdout_sink =
    Eio_unix.pipe sw
  in
  let (stderr_source : Eio_unix.source_ty Eio.Resource.t), stderr_sink =
    Eio_unix.pipe sw
  in
  let stdout_done =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Eio.Flow.copy stdout_source (Eio.Flow.buffer_sink stdout))
  in
  let stderr_done =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Eio.Flow.copy stderr_source (Eio.Flow.buffer_sink stderr))
  in
  let base =
    Eio_unix.Stdenv.override ~stdin ~stdout:stdout_sink ~stderr:stderr_sink env
  in
  let formatter = Stdlib.Format.formatter_of_buffer diagnostics in
  let _ =
    Cmdliner.Cmd.eval_value ~catch:true ~help:formatter ~err:formatter
      ~env:(fun _ -> None)
      ~argv:(Array.of_list ("spin" :: args))
      (Spin.cmd base)
  in
  Eio.Resource.close stdout_sink;
  Eio.Resource.close stderr_sink;
  Eio.Promise.await_exn stdout_done;
  Eio.Promise.await_exn stderr_done;
  Stdlib.Format.pp_print_flush formatter ();
  (Buffer.contents stdout, Buffer.contents stderr)

let test_show_output_non_tty () =
  let stdout, stderr =
    run_non_tty [ "--show-output"; "--"; "sh"; "-c"; "printf out; printf err >&2" ]
  in
  Alcotest.(check bool) "combined output is routed to stdout" true (stream_order stdout);
  Alcotest.(check string) "spinner is not drawn" "Loading...\n" stderr

let test_show_error_non_tty () =
  let stdout, stderr =
    run_non_tty [ "--show-error"; "--"; "sh"; "-c"; "printf out; printf err >&2; exit 7" ]
  in
  Alcotest.(check bool)
    "failed combined output is routed to stdout" true (stream_order stdout);
  Alcotest.(check string) "spinner is not drawn" "Loading...\n" stderr

let test_show_error_success_suppresses_output () =
  let stdout, stderr =
    run_non_tty [ "--show-error"; "--"; "sh"; "-c"; "printf out; printf err >&2" ]
  in
  Alcotest.(check string) "successful output is hidden" "" stdout;
  Alcotest.(check string) "spinner is not drawn" "Loading...\n" stderr

let test_show_stdout_non_tty () =
  let stdout, _ =
    run_non_tty [ "--show-stdout"; "--"; "sh"; "-c"; "printf out; printf err >&2" ]
  in
  Alcotest.(check string) "stdout selection" "out" stdout

let test_show_stderr_non_tty () =
  let stdout, _ =
    run_non_tty [ "--show-stderr"; "--"; "sh"; "-c"; "printf out; printf err >&2" ]
  in
  Alcotest.(check string) "stderr selection" "err" stdout

let test_pty_geometry () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let terminal = Eio_unix.Pty.open_pty ~sw () in
  Eio_unix.Pty.set_window_size (Eio_unix.Pty.tty terminal)
    { Eio_unix.Pty.rows = 17; cols = 53; xpixel = 0; ypixel = 0 };
  let tty_resource =
    Eio_unix.Fd.use_exn "dup" (Eio_unix.Pty.tty terminal) (fun fd -> Unix.dup fd)
    |> fun unix_fd -> Eio_unix.Net.import_socket_stream ~sw ~close_unix:true unix_fd
  in
  let base =
    Eio_unix.Stdenv.override
      ~stdin:(tty_resource :> Eio_unix.source_ty Eio.Resource.t)
      ~stdout:(tty_resource :> Eio_unix.sink_ty Eio.Resource.t)
      ~stderr:(tty_resource :> Eio_unix.sink_ty Eio.Resource.t)
      env
  in
  match
    Spin.run_child base ~command:[ "sh"; "-c"; "stty size; stty size >&2" ] ~timeout:None
  with
  | Error message -> Alcotest.fail message
  | Ok result ->
      Alcotest.(check int) "status" 0 result.Spin.status;
      Alcotest.(check string)
        "stdout child terminal size" "17 53"
        (String.trim result.Spin.stdout);
      Alcotest.(check string)
        "stderr child terminal size" "17 53"
        (String.trim result.Spin.stderr)

let test_child_timeout () =
  Eio_main.run @@ fun env ->
  match Spin.run_child env ~command:[ "sh"; "-c"; "sleep 1" ] ~timeout:(Some 0.01) with
  | Error message -> Alcotest.fail message
  | Ok result ->
      Alcotest.(check bool) "timeout" true result.Spin.timed_out;
      Alcotest.(check int) "timeout status" 124 result.Spin.status

let test_pty_stdin_passthrough () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let terminal = Eio_unix.Pty.open_pty ~sw () in
  Eio_unix.Pty.set_window_size (Eio_unix.Pty.tty terminal)
    { Eio_unix.Pty.rows = 17; cols = 53; xpixel = 0; ypixel = 0 };
  let tty_resource =
    Eio_unix.Fd.use_exn "dup" (Eio_unix.Pty.tty terminal) (fun fd -> Unix.dup fd)
    |> fun unix_fd -> Eio_unix.Net.import_socket_stream ~sw ~close_unix:true unix_fd
  in
  let (stdin_r : Eio_unix.source_ty Eio.Resource.t), stdin_w = Eio_unix.pipe sw in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Flow.copy_string "hello-from-real-stdin\n" stdin_w;
      Eio.Flow.close stdin_w);
  let base =
    Eio_unix.Stdenv.override ~stdin:stdin_r
      ~stdout:(tty_resource :> Eio_unix.sink_ty Eio.Resource.t)
      ~stderr:(tty_resource :> Eio_unix.sink_ty Eio.Resource.t)
      env
  in
  match
    Spin.run_child base
      ~command:[ "sh"; "-c"; "read line; printf 'GOT:%s' \"$line\"" ]
      ~timeout:None
  with
  | Error message -> Alcotest.fail message
  | Ok result ->
      Alcotest.(check int) "status" 0 result.Spin.status;
      Alcotest.(check string)
        "stdin passthrough" "GOT:hello-from-real-stdin"
        (String.trim result.Spin.stdout)

let cases =
  [
    Alcotest.test_case "child capture" `Quick test_child_capture;
    Alcotest.test_case "large child capture" `Quick
      test_child_capture_beyond_memory_buffer;
    Alcotest.test_case "show output in pipe mode" `Quick test_show_output_non_tty;
    Alcotest.test_case "show error in pipe mode" `Quick test_show_error_non_tty;
    Alcotest.test_case "show error success suppression" `Quick
      test_show_error_success_suppresses_output;
    Alcotest.test_case "show stdout in pipe mode" `Quick test_show_stdout_non_tty;
    Alcotest.test_case "show stderr in pipe mode" `Quick test_show_stderr_non_tty;
    Alcotest.test_case "PTY geometry" `Quick test_pty_geometry;
    Alcotest.test_case "timeout" `Quick test_child_timeout;
    Alcotest.test_case "PTY stdin passthrough" `Quick test_pty_stdin_passthrough;
  ]
