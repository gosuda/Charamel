let executable () =
  let test_path = Unix.realpath Sys.executable_name in
  let test_dir = Filename.dirname test_path in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "bin/hotdiva2000/main.exe"

let source_root () =
  Option.value (Sys.getenv_opt "DUNE_SOURCEROOT") ~default:(Sys.getcwd ())
  |> Unix.realpath

let path_env = "/usr/bin:/bin"

let fixture_parent env =
  let root =
    Filename.concat
      (Filename.concat (source_root ()) ".outline")
      (Filename.concat "worktree" "sandboxfixtures")
  in
  Eio.Path.(env#fs / root)

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

let with_fixture env f =
  let parent = fixture_parent env in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 parent;
  let path =
    Filename.temp_file ~temp_dir:(Eio.Path.native_exn parent) "charamel-hotdiva-cli-"
      ".dir"
  in
  Sys.remove path;
  let root = Eio.Path.(env#fs / path) in
  Eio.Path.mkdir ~perm:0o700 root;
  List.iter
    (fun name -> Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / name))
    [ "home"; "xdg-config"; "xdg-data"; "xdg-state"; "xdg-cache"; "tmp" ];
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)
    (fun () -> f path)

let run_cli args =
  Eio_main.run (fun env ->
      with_fixture env (fun root ->
          let error_buffer = Buffer.create 128 in
          let status = ref None in
          let output =
            Eio.Time.with_timeout_exn env#clock 5. (fun () ->
                Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
                  ~env:(minimal_environment ~root) ~stdin:(Eio.Flow.string_source "")
                  ~stderr:(Eio.Flow.buffer_sink error_buffer)
                  ~is_success:(fun code ->
                    status := Some code;
                    true)
                  (executable () :: args))
          in
          let status = Option.value !status ~default:127 in
          (status, output, Buffer.contents error_buffer)))

let output_lines output =
  match List.rev (String.split_on_char '\n' output) with
  | "" :: rest -> List.rev rest
  | rest -> List.rev rest

let contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search offset =
    if needle_length = 0 then true
    else if offset + needle_length > haystack_length then false
    else if String.sub haystack offset needle_length = needle then true
    else search (offset + 1)
  in
  search 0

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
  Alcotest.test_case name `Quick (fun () ->
      let status, output, error = run_cli args in
      Alcotest.(check int) "exit status" 0 status;
      Alcotest.(check string) "stderr" "" error;
      check output)

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
  Alcotest.test_case "tokens lower bound" `Quick (fun () ->
      let status, output, error = run_cli [ "--tokens"; "0" ] in
      Alcotest.(check int) "exit status" 1 status;
      Alcotest.(check string) "output" "" output;
      Alcotest.(check bool) "diagnostic" true (contains ~needle:"must be >= 1" error))

let invalid_count =
  Alcotest.test_case "count lower bound" `Quick (fun () ->
      let status, output, error = run_cli [ "-n-1" ] in
      Alcotest.(check int) "exit status" 1 status;
      Alcotest.(check string) "output" "" output;
      Alcotest.(check bool) "diagnostic" true (contains ~needle:"must be >= 0" error))

let invalid_syntax =
  Alcotest.test_case "invalid integer is a usage error" `Quick (fun () ->
      let status, output, error = run_cli [ "--tokens"; "not-an-integer" ] in
      Alcotest.(check int) "exit status" 2 status;
      Alcotest.(check string) "output" "" output;
      Alcotest.(check bool) "diagnostic" true (String.length error > 0))

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
