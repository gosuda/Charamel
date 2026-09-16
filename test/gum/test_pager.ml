let sanitize_backspaces () =
  Alcotest.(check string) "backspace overwrite" "red" (Pager.sanitize "redx\b")

let find_lines () =
  Alcotest.(check (list int))
    "matching lines" [ 0; 2 ]
    (Pager.search_lines ~pattern:"foo" "foo\nbar\nFOO")

let cases =
  [
    Alcotest.test_case "sanitize backspaces" `Quick sanitize_backspaces;
    Alcotest.test_case "find search lines" `Quick find_lines;
  ]
