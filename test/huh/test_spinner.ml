open Lwt.Syntax

let spinner_child () =
  let* outcome =
    Charamel_huh.Spinner.run ~clock:Charamel_os.Time.lwt (fun () ->
        Unix.sleep 20;
        Ok ())
  in
  let label =
    match outcome with
    | Ok () -> "finished"
    | Error `Interrupted -> "interrupted"
    | Error (`Failed _) -> "failed"
  in
  Printf.printf "SPINNER-STATUS %s\n" label;
  Lwt.return (if String.equal label "interrupted" then 0 else 1)

let run_pty_child () = exit (Lwt_main.run (spinner_child ()))

let with_non_dumb_term f =
  let previous = Sys.getenv_opt "TERM" in
  Unix.putenv "TERM" "xterm-256color";
  Lwt.finalize f (fun () ->
      Unix.putenv "TERM" (Option.value previous ~default:"");
      Lwt.return_unit)

let test_accessible_success () =
  let* outcome =
    Charamel_huh.Spinner.run ~accessible:true ~clock:Charamel_os.Time.lwt (fun () ->
        Ok 42)
  in
  (match outcome with
  | Ok value -> Alcotest.(check int) "action result" 42 value
  | Error `Interrupted -> Alcotest.fail "accessible spinner interrupted"
  | Error (`Failed _) -> Alcotest.fail "accessible spinner failed");
  Lwt.return_unit

let test_accessible_error () =
  let* outcome =
    Charamel_huh.Spinner.run ~accessible:true ~clock:Charamel_os.Time.lwt (fun () ->
        Error "failed action")
  in
  (match outcome with
  | Error (`Failed message) ->
      Alcotest.(check string) "action error" "failed action" message
  | Ok _ -> Alcotest.fail "failed action returned Ok"
  | Error `Interrupted -> Alcotest.fail "action error was reported as interrupt");
  Lwt.return_unit

let rec collect pty attempts acc =
  if attempts <= 0 then Lwt.return acc
  else
    let* chunk = Charamel_os.Pty.read pty 4096 in
    match chunk with
    | Error _ | Ok "" -> Lwt.return acc
    | Ok text ->
        let acc = acc ^ text in
        if Test_support.contains ~needle:"SPINNER-STATUS" ~haystack:acc then
          Lwt.return acc
        else collect pty (attempts - 1) acc

let rec wait_for_frame pty attempts =
  if attempts <= 0 then Lwt.return false
  else
    let* chunk = Charamel_os.Pty.read pty 4096 in
    match chunk with
    | Error _ | Ok "" -> Lwt.return false
    | Ok text ->
        if Test_support.contains ~needle:"Loading..." ~haystack:text then Lwt.return true
        else wait_for_frame pty (attempts - 1)

let interrupt_child pty =
  let* painted = wait_for_frame pty 200 in
  if not painted then Alcotest.fail "spinner never painted a frame"
  else
    let* written = Charamel_os.Pty.write pty "\003" 0 1 in
    match written with
    | Error (`Error message) -> Alcotest.failf "ctrl-c byte rejected: %s" message
    | Error `Unsupported -> Alcotest.skip ()
    | Ok _ ->
        let* report = collect pty 400 "" in
        if Test_support.contains ~needle:"SPINNER-STATUS interrupted" ~haystack:report
        then Lwt.return `Interrupted
        else Alcotest.failf "ctrl-c did not stop the spinner: %S" report

let start_child pty =
  let* spawned =
    Charamel_os.Pty.exec pty ~env:[| "CHARM_TEST_HUH_SPINNER_CHILD=1" |] [ Sys.argv.(0) ]
  in
  match spawned with
  | Error `Unsupported -> Lwt.return_none
  | Error (`Error message) -> Alcotest.failf "child did not start: %s" message
  | Ok _pid -> Lwt.return (Some pty)

let run_ctrl_c_case () =
  with_non_dumb_term (fun () ->
      let* created = Charamel_os.Pty.create ~rows:24 ~cols:80 () in
      match created with
      | Error `Unsupported -> Alcotest.skip ()
      | Error (`Error message) -> Alcotest.failf "pseudo-terminal unavailable: %s" message
      | Ok pty ->
          Lwt.finalize
            (fun () ->
              let* started = start_child pty in
              if started = None then Alcotest.skip ()
              else
                let* _outcome = interrupt_child pty in
                Lwt.return_unit)
            (fun () ->
              Charamel_os.Pty.close pty;
              Lwt.return_unit))

let tests =
  [
    Alcotest_lwt.test_case "accessible success" `Quick (fun _switch () ->
        test_accessible_success ());
    Alcotest_lwt.test_case "action error" `Quick (fun _switch () ->
        test_accessible_error ());
    Alcotest_lwt.test_case "ctrl-c cancels action" `Quick (fun _switch () ->
        run_ctrl_c_case ());
  ]
