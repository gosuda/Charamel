open Charamel_fantasy

let policy = Retry.default

let test_retryable_status () =
  Alcotest.(check bool) "408 retryable" true (Retry.retryable_status 408);
  Alcotest.(check bool) "409 retryable" true (Retry.retryable_status 409);
  Alcotest.(check bool) "429 retryable" true (Retry.retryable_status 429);
  Alcotest.(check bool) "500 retryable" true (Retry.retryable_status 500);
  Alcotest.(check bool) "599 retryable" true (Retry.retryable_status 599);
  Alcotest.(check bool) "400 not retryable" false (Retry.retryable_status 400);
  Alcotest.(check bool) "404 not retryable" false (Retry.retryable_status 404);
  Alcotest.(check bool) "200 not retryable" false (Retry.retryable_status 200)

let test_retry_after_seconds () =
  let delay = Retry.delay policy ~attempt:1 ~retry_after:[ ("Retry-After", "3.5") ] in
  Alcotest.(check (float 0.0001)) "Retry-After wins" 3.5 delay

let test_retry_after_ms () =
  let delay = Retry.delay policy ~attempt:1 ~retry_after:[ ("retry-after-ms", "250") ] in
  Alcotest.(check (float 0.0001)) "retry-after-ms wins in seconds" 0.25 delay

let test_backoff_bounds () =
  let zero_rng () = 0. in
  let delay = Retry.delay ~rng:zero_rng policy ~attempt:1 ~retry_after:[] in
  (* jitter 0.25 with draw 0 scales by 1 - 0.25 = 0.75 *)
  Alcotest.(check (float 0.0001)) "base * (1 - jitter)" (0.5 *. 0.75) delay

let cases =
  [
    Alcotest_lwt.test_case_sync "retryable status" `Quick test_retryable_status;
    Alcotest_lwt.test_case_sync "retry-after seconds" `Quick test_retry_after_seconds;
    Alcotest_lwt.test_case_sync "retry-after ms" `Quick test_retry_after_ms;
    Alcotest_lwt.test_case_sync "backoff bounds" `Quick test_backoff_bounds;
  ]
