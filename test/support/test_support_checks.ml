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

let getenv_of pairs name = List.assoc_opt name pairs

let windows_parent =
  [
    ("PATH", "C:\\toolchain\\bin");
    ("SystemRoot", "C:\\Windows");
    ("SystemDrive", "C:");
    ("PATHEXT", ".COM;.EXE;.BAT;.CMD");
  ]

let lookup name entries =
  List.find_map
    (fun entry ->
      match String.index_opt entry '=' with
      | Some index when String.equal (String.sub entry 0 index) name ->
          Some (String.sub entry (index + 1) (String.length entry - index - 1))
      | _ -> None)
    entries

let count name entries =
  List.length
    (List.filter
       (fun entry ->
         match String.index_opt entry '=' with
         | Some index -> String.equal (String.sub entry 0 index) name
         | None -> false)
       entries)

let checks_posix_passes_the_environment_through () =
  let env = [| "PATH=/usr/bin:/bin"; "HOME=/tmp/fake-home" |] in
  Alcotest.(check bool)
    "a POSIX child keeps the caller array" true
    (Test_support.child_environment ~win32:false ~getenv:(getenv_of windows_parent) env
    == env)

let checks_windows_restores_the_loader_variables () =
  let entries =
    Array.to_list
      (Test_support.child_environment ~win32:true ~getenv:(getenv_of windows_parent)
         [| "PATH=/usr/bin:/bin"; "HOME=/tmp/fake-home" |])
  in
  Alcotest.(check (option string))
    "the inherited PATH replaces the POSIX one" (Some "C:\\toolchain\\bin")
    (lookup "PATH" entries);
  Alcotest.(check (option string))
    "HOME isolation survives" (Some "/tmp/fake-home") (lookup "HOME" entries);
  Alcotest.(check (option string))
    "SystemRoot is inherited" (Some "C:\\Windows") (lookup "SystemRoot" entries);
  Alcotest.(check (option string))
    "SystemDrive is inherited" (Some "C:")
    (lookup "SystemDrive" entries);
  Alcotest.(check (option string))
    "PATHEXT is inherited" (Some ".COM;.EXE;.BAT;.CMD") (lookup "PATHEXT" entries);
  Alcotest.(check int) "PATH occurs once" 1 (count "PATH" entries)

let checks_windows_keeps_a_loader_variable_without_a_parent_value () =
  let entries =
    Array.to_list
      (Test_support.child_environment ~win32:true ~getenv:(getenv_of [])
         [| "PATH=/usr/bin:/bin" |])
  in
  Alcotest.(check (option string))
    "a caller PATH without a parent value survives" (Some "/usr/bin:/bin")
    (lookup "PATH" entries)

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
      ( "child-environment",
        [
          Alcotest.test_case "POSIX passes the caller array through" `Quick
            checks_posix_passes_the_environment_through;
          Alcotest.test_case "Windows restores the loader variables" `Quick
            checks_windows_restores_the_loader_variables;
          Alcotest.test_case "Windows keeps a loader variable without a parent value"
            `Quick checks_windows_keeps_a_loader_variable_without_a_parent_value;
        ] );
    ]
