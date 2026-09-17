let with_non_dumb_term f =
  let previous = Sys.getenv_opt "TERM" in
  Unix.putenv "TERM" "xterm-256color";
  Fun.protect f ~finally:(fun () ->
      Unix.putenv "TERM" (Option.value previous ~default:""))

let test_accessible_success () =
  Eio_main.run (fun env ->
      match
        Charamel_huh.Spinner.run ~accessible:true ~clock:env#clock (fun () -> Ok 42) env
      with
      | Ok value -> Alcotest.(check int) "action result" 42 value
      | Error `Interrupted -> Alcotest.fail "accessible spinner interrupted"
      | Error (`Failed _) -> Alcotest.fail "accessible spinner failed")

let test_accessible_error () =
  Eio_main.run (fun env ->
      match
        Charamel_huh.Spinner.run ~accessible:true ~clock:env#clock
          (fun () -> Error "failed action")
          env
      with
      | Error (`Failed message) ->
          Alcotest.(check string) "action error" "failed action" message
      | Ok _ -> Alcotest.fail "failed action returned Ok"
      | Error `Interrupted -> Alcotest.fail "action error was reported as interrupt")

let test_ctrl_c_cancels_action () =
  with_non_dumb_term (fun () ->
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
                { rows = 24; cols = 80; xpixel = 0; ypixel = 0 };
              let base =
                Eio_unix.Stdenv.override ~stdin:slave_source ~stdout:slave_sink
                  ~stderr:slave_sink env
              in
              let cancelled = ref false in
              let promise, resolver = Eio.Promise.create () in
              Eio.Fiber.fork ~sw (fun () ->
                  let result =
                    Charamel_huh.Spinner.run ~clock:env#clock
                      (fun () ->
                        try
                          Eio.Time.sleep env#clock 60.;
                          Ok ()
                        with Eio.Cancel.Cancelled _ as cancellation ->
                          cancelled := true;
                          raise cancellation)
                      base
                  in
                  Eio.Promise.resolve resolver result);
              Eio.Time.sleep env#clock 0.05;
              Eio.Flow.copy_string "\003" (Eio_unix.Pty.sink pty);
              match
                Eio.Time.with_timeout_exn env#clock 5. (fun () ->
                    Eio.Promise.await promise)
              with
              | Error `Interrupted ->
                  Alcotest.(check bool) "action fiber cancelled" true !cancelled
              | Error (`Failed _) -> Alcotest.fail "ctrl-c reported action failure"
              | Ok () -> Alcotest.fail "ctrl-c did not stop spinner")))

let tests =
  [
    Alcotest.test_case "accessible success" `Quick test_accessible_success;
    Alcotest.test_case "action error" `Quick test_accessible_error;
    Alcotest.test_case "ctrl-c cancels action" `Quick test_ctrl_c_cancels_action;
  ]
