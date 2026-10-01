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

let timeout_boundaries () =
  let exact, _ = Timer.update Timer.Tick (Timer.v ~interval:1.0 ~timeout:1.0 ()) in
  Alcotest.(check (float 0.0001))
    "a tick landing on zero is timed out" 0.0 (Timer.timeout exact);
  Alcotest.(check bool) "zero counts as timed out" true (Timer.timed_out exact);
  let over, _ = Timer.update Timer.Tick (Timer.v ~interval:1.5 ~timeout:1.0 ()) in
  Alcotest.(check bool)
    "an overshooting tick goes negative" true
    (Timer.timeout over < 0.0);
  Alcotest.(check bool) "a negative timeout is timed out" true (Timer.timed_out over);
  Alcotest.(check bool) "a timed-out timer is not running" false (Timer.running over);
  let again, _ = Timer.update Timer.Tick over in
  Alcotest.(check (float 0.0001))
    "ticks after the timeout do not advance it" (-0.5) (Timer.timeout again);
  let restarted, _ = Timer.update Timer.Start again in
  Alcotest.(check bool)
    "start cannot revive a timed-out timer" false (Timer.running restarted)

let cases =
  [
    Alcotest_lwt.test_case_sync "transitions" `Quick transitions;
    Alcotest_lwt.test_case_sync "toggle and view" `Quick toggle_and_view;
    Alcotest_lwt.test_case_sync "timeout boundaries" `Quick timeout_boundaries;
  ]
