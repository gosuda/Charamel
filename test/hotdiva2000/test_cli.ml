let executable () =
  let test_path = Unix.realpath Sys.executable_name in
  let test_dir = Filename.dirname test_path in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "bin/hotdiva2000/main.exe"

let path_env = "/usr/bin:/bin"

let minimal_environment ~root =
  let path name = Filename.concat root name in
  [|
    "PATH=" ^ path_env;
    "LANG=C";
    "TERM=xterm-256color";
    "HOME=" ^ path "home";
    "XDG_CONFIG_HOME=" ^ path "xdg-config";
    "XDG_DATA_HOME=" ^ path "xdg-data";
    "XDG_STATE_HOME=" ^ path "xdg-state";
    "XDG_CACHE_HOME=" ^ path "xdg-cache";
    "TMPDIR=" ^ path "tmp";
  |]

let output_lines output =
  match List.rev (String.split_on_char '\n' output) with
  | "" :: rest -> List.rev rest
  | rest -> List.rev rest

let count_occurrences ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  if needle_length = 0 then 0
  else
    let rec count offset total =
      if offset + needle_length > haystack_length then total
      else if String.sub haystack offset needle_length = needle then
        count (offset + needle_length) (total + 1)
      else count (offset + 1) total
    in
    count 0 0

let check_line ~separator ~tokens index line =
  Alcotest.(check bool)
    (Fmt.str "line %d is non-empty" index)
    true
    (String.length line > 0);
  Alcotest.(check string)
    (Fmt.str "line %d is lowercase" index)
    line (String.lowercase_ascii line);
  Alcotest.(check bool)
    (Fmt.str "line %d has no spaces" index)
    false (String.contains line ' ');
  if separator <> "" then
    Alcotest.(check bool)
      (Fmt.str "line %d has token separators" index)
      true
      (count_occurrences ~needle:separator line >= tokens - 1)

let expect_success name args check =
  Alcotest_lwt.test_case name `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let open Lwt.Syntax in
          let* status, output, error =
            Test_support.run_cli ~exe:(executable ()) ~env:(minimal_environment ~root)
              ~timeout:5. args
          in
          Alcotest.(check int) "exit status" 0 status;
          Alcotest.(check string) "stderr" "" error;
          check output;
          Lwt.return_unit))

let default_generation =
  expect_success "default generates one name" [] (fun output ->
      match output_lines output with
      | [ line ] -> check_line ~separator:"-" ~tokens:2 0 line
      | _ -> Alcotest.fail "default invocation did not print exactly one line")

let option_generation =
  expect_success "count, separator, and tokens"
    [ "-n"; "4"; "--separator"; "|"; "--tokens"; "3" ] (fun output ->
      let lines = output_lines output in
      Alcotest.(check int) "line count" 4 (List.length lines);
      List.iteri (check_line ~separator:"|" ~tokens:3) lines)

let zero_count =
  expect_success "zero count is pipe-safe" [ "-n"; "0" ] (fun output ->
      Alcotest.(check string) "output" "" output)

let many_lines =
  expect_success "all generated lines reach a pipe" [ "-n"; "64" ] (fun output ->
      let lines = output_lines output in
      Alcotest.(check int) "line count" 64 (List.length lines);
      List.iteri (check_line ~separator:"-" ~tokens:2) lines)

let invalid_tokens =
  Alcotest_lwt.test_case "tokens lower bound" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let open Lwt.Syntax in
          let* status, output, error =
            Test_support.run_cli ~exe:(executable ()) ~env:(minimal_environment ~root)
              ~timeout:5. [ "--tokens"; "0" ]
          in
          Alcotest.(check int) "exit status" 1 status;
          Alcotest.(check string) "output" "" output;
          Alcotest.(check bool)
            "diagnostic" true
            (Test_support.contains ~needle:"must be >= 1" ~haystack:error);
          Lwt.return_unit))

let invalid_count =
  Alcotest_lwt.test_case "count lower bound" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let open Lwt.Syntax in
          let* status, output, error =
            Test_support.run_cli ~exe:(executable ()) ~env:(minimal_environment ~root)
              ~timeout:5. [ "-n-1" ]
          in
          Alcotest.(check int) "exit status" 1 status;
          Alcotest.(check string) "output" "" output;
          Alcotest.(check bool)
            "diagnostic" true
            (Test_support.contains ~needle:"must be >= 0" ~haystack:error);
          Lwt.return_unit))

let invalid_syntax =
  Alcotest_lwt.test_case "invalid integer is a usage error" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let open Lwt.Syntax in
          let* status, output, _ =
            Test_support.run_cli ~exe:(executable ()) ~env:(minimal_environment ~root)
              ~timeout:5.
              [ "--tokens"; "not-an-integer" ]
          in
          Alcotest.(check int) "exit status" 2 status;
          Alcotest.(check string) "output" "" output;
          Lwt.return_unit))

let suites =
  [
    ( "cli",
      [
        default_generation;
        option_generation;
        zero_count;
        many_lines;
        invalid_tokens;
        invalid_count;
        invalid_syntax;
      ] );
  ]
