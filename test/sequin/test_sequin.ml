let executable () =
  let test_path = Unix.realpath Sys.executable_name in
  let test_dir = Filename.dirname test_path in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "bin/sequin/main.exe"

let expect_success ?(stdin = "") name args expected =
  Alcotest.test_case name `Quick (fun () ->
      let exe = executable () in
      let status, output, error = Test_support.run_cli ~exe ~stdin args in
      Alcotest.(check int) "exit status" 0 status;
      Alcotest.(check string) "stderr" "" error;
      Alcotest.(check string) "stdout" expected output)

let expect_failure ?(stdin = "") name args expected_status message =
  Alcotest.test_case name `Quick (fun () ->
      let exe = executable () in
      let status, output, error = Test_support.run_cli ~exe ~stdin args in
      Alcotest.(check int) "exit status" expected_status status;
      Alcotest.(check string) "stdout" "" output;
      Alcotest.(check bool)
        "diagnostic" true
        (Test_support.contains ~needle:message ~haystack:error))

let with_temp_file contents f =
  Test_support.with_temp_dir (fun dir ->
      let path =
        Filename.temp_file ~temp_dir:(Eio.Path.native_exn dir) "sequin-" ".ansi"
      in
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      f path)

let cases =
  [
    expect_success ~stdin:"hello" "explains piped input" [] "Print \"hello\"\n";
    expect_success ~stdin:"\x1b[31mred\x1b[0m" "prints escaped bytes" [ "--raw" ]
      "\\027[31mred\\027[0m";
    expect_success ~stdin:"\x1b[38;2;255;0;0m" "prints one explanation per action" []
      "CSI 38;2;255;0;0 m  SGR: foreground color: rgb 255 0 0\n";
    expect_success ~stdin:"\xF0\x9F\x91\x8D\xF0\x9F\x8F\xBD"
      "width handles emoji modifiers" [ "--width" ] "2\n";
    expect_success ~stdin:"\xEF\xBD\xB6\xEF\xBE\x9E" "width handles halfwidth marks"
      [ "--width" ] "1\n";
    expect_success ~stdin:"\x61\xCC\x80" "width handles combining marks" [ "--width" ]
      "1\n";
    expect_success ~stdin:(String.make 100_001 'a') "width reads all finite input"
      [ "--width" ] "100001\n";
    expect_failure ~stdin:"a" "raw and width are exclusive" [ "--raw"; "--width" ] 2
      "--raw and --width";
    expect_failure "missing file is a command error"
      [ "/definitely/not/a/sequin-input" ]
      1 "no such file or directory";
    Alcotest.test_case "reads a file argument" `Quick (fun () ->
        with_temp_file "file input" (fun path ->
            let exe = executable () in
            let status, output, error = Test_support.run_cli ~exe [ path ] in
            Alcotest.(check int) "exit status" 0 status;
            Alcotest.(check string) "stderr" "" error;
            Alcotest.(check string) "stdout" "Print \"file input\"\n" output));
  ]

let () = Alcotest.run "sequin" [ ("explain", Test_explain.cases); ("cli", cases) ]
