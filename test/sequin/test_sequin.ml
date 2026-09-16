let executable () =
  let test_path = Unix.realpath Sys.executable_name in
  let test_dir = Filename.dirname test_path in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "bin/sequin/main.exe"

let run_cli ?(stdin = "") args =
  Eio_main.run (fun env ->
      let error_buffer = Buffer.create 128 in
      let status = ref None in
      let output =
        Eio.Time.with_timeout_exn env#clock 5. (fun () ->
            Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
              ~stdin:(Eio.Flow.string_source stdin)
              ~stderr:(Eio.Flow.buffer_sink error_buffer)
              ~is_success:(fun code ->
                status := Some code;
                true)
              (executable () :: args))
      in
      (Option.value !status ~default:127, output, Buffer.contents error_buffer))

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

let expect_success ?(stdin = "") name args expected =
  Alcotest.test_case name `Quick (fun () ->
      let status, output, error = run_cli ~stdin args in
      Alcotest.(check int) "exit status" 0 status;
      Alcotest.(check string) "stderr" "" error;
      Alcotest.(check string) "stdout" expected output)

let expect_failure ?(stdin = "") name args expected_status message =
  Alcotest.test_case name `Quick (fun () ->
      let status, output, error = run_cli ~stdin args in
      Alcotest.(check int) "exit status" expected_status status;
      Alcotest.(check string) "stdout" "" output;
      Alcotest.(check bool) "diagnostic" true (contains ~needle:message error))

let source_root () =
  Option.value (Sys.getenv_opt "DUNE_SOURCEROOT") ~default:(Sys.getcwd ())
  |> Unix.realpath

let fixture_dir () =
  Filename.concat (Filename.concat (source_root ()) ".outline") "worktree"

let rec mkdir_p path =
  if not (Sys.file_exists path) then begin
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let with_temp_file contents f =
  let dir = fixture_dir () in
  mkdir_p dir;
  let path = Filename.temp_file ~temp_dir:dir "sequin-" ".ansi" in
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr channel;
      try Sys.remove path with Sys_error _ -> ())
    (fun () ->
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
            let status, output, error = run_cli [ path ] in
            Alcotest.(check int) "exit status" 0 status;
            Alcotest.(check string) "stderr" "" error;
            Alcotest.(check string) "stdout" "Print \"file input\"\n" output));
  ]

let () = Alcotest.run "sequin" [ ("explain", Test_explain.cases); ("cli", cases) ]
