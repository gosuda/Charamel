open Lwt.Syntax

let printf_verbs () =
  let value =
    Log.format_message "name=%s count=%d value=%v quoted=%q %%"
      [ "gum"; "3"; "ok"; "a b" ]
  in
  Alcotest.(check string)
    "printf-compatible verbs" "name=gum count=3 value=ok quoted=\"a b\" %" value

let unknown_verbs_stay_literal () =
  Alcotest.(check string) "unknown verb" "x=%z" (Log.format_message "x=%z" [ "ignored" ])

let time_presets () =
  let time =
    match Ptime.of_rfc3339 "2026-09-16T13:05:07.123Z" with
    | Ok (value, _, _) -> value
    | Error _ -> Alcotest.fail "invalid test timestamp"
  in
  Alcotest.(check string)
    "rfc3339" "2026-09-16T13:05:07Z"
    (Log.time_formatter "rfc3339" time);
  Alcotest.(check string)
    "rfc3339nano" "2026-09-16T13:05:07.123Z"
    (Log.time_formatter "rfc3339nano" time);
  Alcotest.(check string) "kitchen" "1:05PM" (Log.time_formatter "kitchen" time);
  Alcotest.(check string)
    "custom year" "2026/09/16"
    (Log.time_formatter "2006/01/02" time)

let default_env () =
  {
    Charamel_cli.Env.cwd = Unix.getcwd ();
    fs_root = "/";
    stdin = Lwt_io.stdin;
    stdout = Lwt_io.stdout;
    stderr = Lwt_io.stderr;
    clock = Charamel_os.Time.lwt;
  }

let read_file path =
  let channel = open_in_bin path in
  let length = in_channel_length channel in
  let content = really_input_string channel length in
  close_in channel;
  content

let with_file ?(structured = false) formatter level fields =
  (* [Filename.temp_file] lands in the platform temp dir: a literal [/tmp] is a
     drive-relative path on Windows and its parent may not exist there. *)
  let path = Filename.temp_file "gum-log-test" "" in
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  let* () =
    Log.emit ~file:path ~formatter ~level ~structured ~prefix:"app" (default_env ())
      fields
  in
  let output = read_file path in
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  Lwt.return output

let reporter_formats () =
  let* text = with_file Log.Text Log.Info [ "hello" ] in
  let* logfmt =
    with_file ~structured:true Log.Logfmt Log.Info [ "hello"; "key"; "value" ]
  in
  let* json = with_file ~structured:true Log.Json Log.Info [ "hello"; "key"; "value" ] in
  Alcotest.(check string) "text output" "INFO  app: hello\n" text;
  Alcotest.(check string)
    "logfmt output" "level=info prefix=app msg=hello key=value\n" logfmt;
  Alcotest.(check string)
    "json output"
    "{\"level\":\"info\",\"prefix\":\"app\",\"msg\":\"hello\",\"key\":\"value\"}\n" json;
  Lwt.return_unit

let minimum_filters () =
  let path = Fmt.str "/tmp/gum-log-filter-%d" (Unix.getpid ()) in
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  let* () =
    Log.emit ~file:path ~level:Log.Info ~min_level:"error" (default_env ()) [ "hidden" ]
  in
  Alcotest.(check bool)
    "filtered record does not create output" false (Sys.file_exists path);
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  Lwt.return_unit

let cases =
  [
    Alcotest_lwt.test_case_sync "printf verbs" `Quick printf_verbs;
    Alcotest_lwt.test_case_sync "unknown verbs" `Quick unknown_verbs_stay_literal;
    Alcotest_lwt.test_case_sync "time presets" `Quick time_presets;
    Alcotest_lwt.test_case "reporter formats" `Quick (fun _switch () ->
        reporter_formats ());
    Alcotest_lwt.test_case "minimum level" `Quick (fun _switch () -> minimum_filters ());
  ]
