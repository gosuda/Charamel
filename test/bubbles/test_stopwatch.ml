module Stopwatch = Charamel_bubbles.Stopwatch

let start_stop_reset () =
  let stopwatch = Stopwatch.v ~interval:0.5 () in
  Alcotest.(check bool) "initially stopped" false (Stopwatch.running stopwatch);
  let stopwatch = Stopwatch.start stopwatch in
  let stopwatch, _ = Stopwatch.update Stopwatch.Tick stopwatch in
  let stopwatch, _ = Stopwatch.update Stopwatch.Tick stopwatch in
  Alcotest.(check (float 0.0001)) "elapsed ticks" 1.0 (Stopwatch.elapsed stopwatch);
  let stopwatch = Stopwatch.stop stopwatch in
  let stopwatch, _ = Stopwatch.update Stopwatch.Tick stopwatch in
  Alcotest.(check (float 0.0001)) "stopped value" 1.0 (Stopwatch.elapsed stopwatch);
  let stopwatch = Stopwatch.reset stopwatch in
  Alcotest.(check (float 0.0001)) "reset" 0.0 (Stopwatch.elapsed stopwatch)

let toggle_and_view () =
  let stopwatch = Stopwatch.v ~interval:1.0 () |> Stopwatch.toggle in
  let stopwatch, _ = Stopwatch.update Stopwatch.Tick stopwatch in
  Alcotest.(check string) "duration view" "1s" (Stopwatch.view stopwatch);
  let stopwatch = Stopwatch.toggle stopwatch in
  Alcotest.(check bool) "toggle stops" false (Stopwatch.running stopwatch)

let cases =
  [
    Alcotest_lwt.test_case_sync "start stop reset" `Quick start_stop_reset;
    Alcotest_lwt.test_case_sync "toggle and view" `Quick toggle_and_view;
  ]
