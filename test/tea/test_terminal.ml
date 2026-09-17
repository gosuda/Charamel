module Terminal = Charamel_tea__Terminal

let check_source name source expected =
  let buffer = Cstruct.create (String.length expected) in
  let rec read offset =
    if offset = String.length expected then ()
    else
      let count =
        Eio.Flow.single_read source
          (Cstruct.sub buffer offset (String.length expected - offset))
      in
      read (offset + count)
  in
  read 0;
  Alcotest.(check string) name expected (Cstruct.to_string buffer)

let check_resource_fd name resource expected_fd =
  match Eio_unix.Resource.fd_opt resource with
  | None -> Alcotest.failf "%s does not expose a Unix descriptor" name
  | Some fd ->
      let equal =
        Eio_unix.Fd.use_exn name fd (fun actual ->
            Eio_unix.Fd.use_exn name expected_fd (fun expected -> actual = expected))
      in
      Alcotest.(check bool) name true equal

let test_custom_io_boundary () =
  Eio_main.run (fun _env ->
      let input = Eio.Flow.string_source "input" in
      let output_buffer = Buffer.create 16 in
      let output = Eio.Flow.buffer_sink output_buffer in
      let resize_events = Eio.Stream.create 1 in
      let terminal =
        Terminal.custom ~input ~output
          ~size:(fun () -> (13, 47))
          ~on_resize:(Some resize_events)
          ~env:(function "TERM" -> Some "xterm-256color" | _ -> None)
          ~is_tty:true
      in
      Alcotest.(check bool) "custom tty flag" true terminal.Terminal.is_tty;
      Alcotest.(check (pair int int)) "custom size" (13, 47) (terminal.Terminal.size ());
      Alcotest.(check (option string))
        "custom environment" (Some "xterm-256color")
        (terminal.Terminal.env "TERM");
      Alcotest.(check bool)
        "custom resize stream" true
        (Option.is_some terminal.Terminal.on_resize);
      terminal.Terminal.enter ();
      terminal.Terminal.leave ();
      check_source "custom input" terminal.Terminal.input "input";
      Eio.Flow.copy_string "output" terminal.Terminal.output;
      Alcotest.(check string) "custom output" "output" (Buffer.contents output_buffer))

let test_local_pipe_boundary () =
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          let input, _input_sink = Eio_unix.pipe sw in
          let stdout_source, stdout_sink = Eio_unix.pipe sw in
          let stderr_source, stderr_sink = Eio_unix.pipe sw in
          let local_env =
            Eio_unix.Stdenv.override ~stdin:input ~stdout:stdout_sink ~stderr:stderr_sink
              env
          in
          let stdout_terminal = Terminal.local local_env in
          let stderr_terminal = Terminal.local ~output:`Stderr local_env in
          Alcotest.(check bool) "pipe is not tty" false stdout_terminal.Terminal.is_tty;
          check_resource_fd "local input" stdout_terminal.Terminal.input
            (Eio_unix.Resource.fd input);
          check_resource_fd "stdout output" stdout_terminal.Terminal.output
            (Eio_unix.Resource.fd stdout_sink);
          check_resource_fd "stderr output" stderr_terminal.Terminal.output
            (Eio_unix.Resource.fd stderr_sink);
          stdout_terminal.Terminal.enter ();
          stdout_terminal.Terminal.leave ();
          stdout_terminal.Terminal.leave ();
          let rows, cols = stdout_terminal.Terminal.size () in
          Alcotest.(check bool) "pipe fallback rows" true (rows > 0);
          Alcotest.(check bool) "pipe fallback columns" true (cols > 0);
          Eio.Flow.copy_string "stdout" stdout_terminal.Terminal.output;
          Eio.Flow.copy_string "stderr" stderr_terminal.Terminal.output;
          check_source "stdout data" stdout_source "stdout";
          check_source "stderr data" stderr_source "stderr"))

let test_local_pty_lifecycle () =
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          let pty = Eio_unix.Pty.open_pty ~sw () in
          let slave_fd = Eio_unix.Pty.tty pty in
          let slave_unix_fd =
            Eio_unix.Fd.use_exn "dup" slave_fd (fun fd -> Unix.dup fd)
          in
          let slave_flow =
            Eio_unix.Net.import_socket_stream ~sw ~close_unix:true slave_unix_fd
          in
          let slave_source : Eio_unix.source_ty Eio.Resource.t =
            (slave_flow :> Eio_unix.source_ty Eio.Resource.t)
          in
          let slave_sink : Eio_unix.sink_ty Eio.Resource.t =
            (slave_flow :> Eio_unix.sink_ty Eio.Resource.t)
          in
          Eio_unix.Pty.set_window_size slave_fd
            { rows = 31; cols = 73; xpixel = 0; ypixel = 0 };
          let local_env =
            Eio_unix.Stdenv.override ~stdin:slave_source ~stdout:slave_sink
              ~stderr:slave_sink env
          in
          let before = Eio_unix.Pty.Tc.getattr slave_fd in
          let terminal = Terminal.local local_env in
          let after_create = Eio_unix.Pty.Tc.getattr slave_fd in
          Alcotest.(check bool) "pty detected" true terminal.Terminal.is_tty;
          Alcotest.(check bool)
            "constructor leaves termios unchanged" true (before = after_create);
          let rows, cols = terminal.Terminal.size () in
          Alcotest.(check (pair int int)) "pty dimensions" (31, 73) (rows, cols);
          Fun.protect ~finally:terminal.Terminal.leave (fun () ->
              terminal.Terminal.enter ();
              let raw = Eio_unix.Pty.Tc.getattr slave_fd in
              Alcotest.(check bool) "raw canonical" false raw.Unix.c_icanon;
              Alcotest.(check bool) "raw echo" false raw.Unix.c_echo;
              Alcotest.(check bool) "raw signals" false raw.Unix.c_isig;
              Alcotest.(check bool) "raw output processing" false raw.Unix.c_opost;
              Alcotest.(check bool) "raw software flow control" false raw.Unix.c_ixon;
              Alcotest.(check int) "raw minimum bytes" 1 raw.Unix.c_vmin;
              Alcotest.(check int) "raw timeout" 0 raw.Unix.c_vtime;
              terminal.Terminal.enter ();
              terminal.Terminal.leave ();
              terminal.Terminal.leave ();
              let restored = Eio_unix.Pty.Tc.getattr slave_fd in
              Alcotest.(check bool)
                "leave restores initial termios" true (before = restored))))

let cases =
  [
    Alcotest.test_case "custom IO boundary" `Quick test_custom_io_boundary;
    Alcotest.test_case "local pipe boundary" `Quick test_local_pipe_boundary;
    Alcotest.test_case "local PTY lifecycle" `Quick test_local_pty_lifecycle;
  ]
