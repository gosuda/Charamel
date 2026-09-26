module Timer = Charamel_bubbles.Timer

let transitions () =
  let timer = Timer.v ~interval:1.0 ~timeout:3.0 () in
  Alcotest.(check bool) "starts running" true (Timer.running timer);
  let timer, _ = Timer.update Timer.Tick timer in
  Alcotest.(check (float 0.0001)) "decrements" 2.0 (Timer.timeout timer);
  let timer, _ = Timer.update Timer.Stop timer in
  let timer, _ = Timer.update Timer.Tick timer in
  Alcotest.(check (float 0.0001)) "stopped timer does not tick" 2.0 (Timer.timeout timer);
  let timer, _ = Timer.update Timer.Start timer in
  let timer, _ = Timer.update Timer.Tick timer in
  let timer, _ = Timer.update Timer.Tick timer in
  Alcotest.(check bool) "times out" true (Timer.timed_out timer);
  Alcotest.(check bool) "timed out timer is not running" false (Timer.running timer)

let toggle_and_view () =
  let timer = Timer.v ~interval:0.5 ~timeout:1.5 () in
  let timer = Timer.toggle timer in
  Alcotest.(check bool) "toggle pauses" false (Timer.running timer);
  let timer = Timer.toggle timer in
  Alcotest.(check bool) "toggle resumes" true (Timer.running timer);
  let timer, _ = Timer.update Timer.Tick timer in
  Alcotest.(check string) "duration view" "1s" (Timer.view timer)

let cases =
  [
    Alcotest_lwt.test_case_sync "transitions" `Quick transitions;
    Alcotest_lwt.test_case_sync "toggle and view" `Quick toggle_and_view;
  ]
