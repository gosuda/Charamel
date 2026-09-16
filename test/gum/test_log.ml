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

let with_file ?(structured = false) formatter level fields =
  let path = Fmt.str "/tmp/gum-log-test-%d" (Unix.getpid ()) in
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  Eio_main.run (fun env ->
      Log.emit ~file:path ~formatter ~level ~structured ~prefix:"app" env fields;
      let output = Eio.Path.(load (env#fs / path)) in
      (try Unix.unlink path with Unix.Unix_error _ -> ());
      output)

let reporter_formats () =
  let text = with_file Log.Text Log.Info [ "hello" ] in
  let logfmt =
    with_file ~structured:true Log.Logfmt Log.Info [ "hello"; "key"; "value" ]
  in
  let json = with_file ~structured:true Log.Json Log.Info [ "hello"; "key"; "value" ] in
  Alcotest.(check string) "text output" "INFO  app: hello\n" text;
  Alcotest.(check string)
    "logfmt output" "level=info prefix=app msg=hello key=value\n" logfmt;
  Alcotest.(check string)
    "json output"
    "{\"level\":\"info\",\"prefix\":\"app\",\"msg\":\"hello\",\"key\":\"value\"}\n" json

let minimum_filters () =
  let path = Fmt.str "/tmp/gum-log-filter-%d" (Unix.getpid ()) in
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  Eio_main.run (fun env ->
      Log.emit ~file:path ~level:Log.Info ~min_level:"error" env [ "hidden" ];
      Alcotest.(check bool)
        "filtered record does not create output" false (Sys.file_exists path);
      try Unix.unlink path with Unix.Unix_error _ -> ())

let cases =
  [
    Alcotest.test_case "printf verbs" `Quick printf_verbs;
    Alcotest.test_case "unknown verbs" `Quick unknown_verbs_stay_literal;
    Alcotest.test_case "time presets" `Quick time_presets;
    Alcotest.test_case "reporter formats" `Quick reporter_formats;
    Alcotest.test_case "minimum level" `Quick minimum_filters;
  ]
