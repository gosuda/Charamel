let version text =
  match Semver.parse text with
  | Ok value -> value
  | Error (`Msg message) -> Alcotest.failf "parse %S: %s" text message

let constraint_ text =
  match Semver.parse_constraint text with
  | Ok value -> value
  | Error (`Msg message) -> Alcotest.failf "constraint %S: %s" text message

let accepts constraint_text version_text =
  Alcotest.(check bool)
    constraint_text true
    (Semver.satisfies (constraint_ constraint_text) (version version_text))

let rejects constraint_text version_text =
  Alcotest.(check bool)
    constraint_text false
    (Semver.satisfies (constraint_ constraint_text) (version version_text))

let precedence () =
  Alcotest.(check int)
    "release follows prerelease" 1
    (Semver.compare (version "1.0.0") (version "1.0.0-rc.1"));
  Alcotest.(check int)
    "numeric prerelease identifiers" (-1)
    (Semver.compare (version "1.0.0-alpha.2") (version "1.0.0-alpha.10"))

let wildcards_and_ranges () =
  accepts "1" "1.9.2";
  rejects "1" "2.0.0";
  accepts "1.2.x" "1.2.99";
  rejects "1.2.x" "1.3.0";
  accepts "1.2.3 - 2.3.4" "2.3.4";
  rejects "1.2.3 - 2.3.4" "2.3.5";
  accepts "~1" "1.9.0";
  rejects "~1" "2.0.0";
  accepts "^0" "0.9.0";
  rejects "^0" "1.0.0";
  accepts ">= 1.2 < 2" "1.5.0";
  rejects ">= 1.2 < 2" "2.0.0"

let disjunction_and_prerelease () =
  accepts "<1.0.0 || >=2.0.0" "2.0.0";
  rejects ">=1.0.0" "1.1.0-beta";
  accepts ">=1.0.0-beta" "1.1.0-beta"

let version_command () =
  (match Version_cmd.check ~current:"1.2.3" ">=1.0 <2" with
  | Ok () -> ()
  | Error (`Msg message) -> Alcotest.fail message);
  match Version_cmd.check ~current:"1.2.3" ">2" with
  | Error (`Msg message) ->
      Alcotest.(check bool)
        "mismatch diagnostic" true
        (Test_support.contains ~needle:"is not within given range" ~haystack:message)
  | Ok () -> Alcotest.fail "out-of-range version accepted"

let display_without_constraint () =
  match Version_cmd.display ~current:"1.2.3" None with
  | Ok output -> Alcotest.(check string) "version output" "1.2.3" output
  | Error (`Msg message) -> Alcotest.fail message

let malformed () =
  match Semver.parse_constraint ">=" with
  | Error (`Msg _) -> ()
  | Ok _ -> Alcotest.fail "bare comparator accepted"

let cases =
  [
    Alcotest_lwt.test_case_sync "precedence" `Quick precedence;
    Alcotest_lwt.test_case_sync "wildcards and ranges" `Quick wildcards_and_ranges;
    Alcotest_lwt.test_case_sync "disjunction and prerelease" `Quick
      disjunction_and_prerelease;
    Alcotest_lwt.test_case_sync "version command" `Quick version_command;
    Alcotest_lwt.test_case_sync "version display" `Quick display_without_constraint;
    Alcotest_lwt.test_case_sync "malformed" `Quick malformed;
  ]
