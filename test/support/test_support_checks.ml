(* Self-checks for the shared helpers themselves. Every suite trusts [Test_support.contains]
   to answer "is this text on screen?", so the predicate is pinned from both directions: a
   present needle must report true and an absent needle must report false. Without the
   negative half, an always-true helper would green-light every assertion built on it. The
   fixed CJK corpus is re-measured against the width model it was recorded from, so a row
   cannot drift away from the thing it claims to pin. *)

let substring_cases =
  [
    ("finds an inner needle", true, "cd", "abcde");
    ("finds a prefix", true, "ab", "abcde");
    ("finds a suffix", true, "de", "abcde");
    ("finds the whole string", true, "abcde", "abcde");
    ("finds an overlapping repeat", true, "aba", "ababa");
    ("accepts the empty needle", true, "", "abcde");
    ("rejects an absent needle", false, "xz", "abcde");
    ("rejects a shifted needle", false, "ace", "abcde");
    ("rejects a needle longer than the haystack", false, "abcdef", "abcde");
    ("rejects a needle in an empty haystack", false, "a", "");
  ]

let checks_substring_in_both_directions () =
  List.iter
    (fun (name, expected, needle, haystack) ->
      Alcotest.(check bool) name expected (Test_support.contains ~needle ~haystack))
    substring_cases

let corpus_rows_measure_as_recorded () =
  List.iter
    (fun (text, cells, graphemes) ->
      Alcotest.(check int)
        (Fmt.str "cells of %S" text) cells
        (Charamel_ansi.Width.string_width text);
      Alcotest.(check int)
        (Fmt.str "graphemes of %S" text)
        graphemes
        (List.length (Charamel_ansi.Width.graphemes text)))
    Test_support.corpus

let () =
  Alcotest.run "test-support"
    [
      ( "substring",
        [
          Alcotest.test_case "search answers both ways" `Quick
            checks_substring_in_both_directions;
        ] );
      ( "corpus",
        [
          Alcotest.test_case "the CJK corpus matches the width model" `Quick
            corpus_rows_measure_as_recorded;
        ] );
    ]
