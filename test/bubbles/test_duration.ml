let expected_vectors () =
  let vectors =
    [
      (3723.5, "1h2m3.5s");
      (120.5, "2m0.5s");
      (1.5, "1.5s");
      (0., "0s");
      (0.5, "500ms");
      (1.5e-6, "1.5µs");
      (1.e-7, "100ns");
      (60., "1m0s");
      (3600., "1h0m0s");
    ]
  in
  Stdlib.List.iter
    (fun (seconds, expected) ->
      Alcotest.(check string)
        (string_of_float seconds) expected
        (Charamel_bubbles.Duration.to_string seconds))
    vectors

let trimmed_fraction_and_sign () =
  Alcotest.(check string)
    "trims zero fractional digits" "2s"
    (Charamel_bubbles.Duration.to_string 2.0);
  Alcotest.(check string)
    "nanoseconds are rounded before formatting" "1ns"
    (Charamel_bubbles.Duration.to_string 0.0000000006);
  Alcotest.(check string)
    "negative subsecond" "-1.5µs"
    (Charamel_bubbles.Duration.to_string (-0.0000015));
  Alcotest.(check string)
    "negative minutes" "-1m0.25s"
    (Charamel_bubbles.Duration.to_string (-60.25))

let rejects_nonfinite () =
  let raised =
    try
      ignore (Charamel_bubbles.Duration.to_string Float.nan);
      false
    with Invalid_argument _ -> true
  in
  Alcotest.(check bool) "NaN is rejected" true raised;
  let raised =
    try
      ignore (Charamel_bubbles.Duration.to_string Float.infinity);
      false
    with Invalid_argument _ -> true
  in
  Alcotest.(check bool) "infinity is rejected" true raised

let cases =
  [
    Alcotest.test_case "expected vectors" `Quick expected_vectors;
    Alcotest.test_case "fraction and sign" `Quick trimmed_fraction_and_sign;
    Alcotest.test_case "non-finite values" `Quick rejects_nonfinite;
  ]
