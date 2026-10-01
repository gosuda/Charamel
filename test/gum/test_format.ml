let expect_ok name = function
  | Ok value -> value
  | Error (`Msg message) -> Alcotest.failf "%s: %s" name message

let template_styles_text () =
  let rendered =
    expect_ok "template" (Format.render Format.Template "{{ Bold \"hello\" }}")
  in
  Alcotest.(check string) "bold template" "\027[1mhello\027[m" rendered

let nested_and_trim () =
  let rendered =
    expect_ok "nested"
      (Format.render Format.Template "left  {{- Bold (Foreground \"99\" \"x\") -}}  right")
  in
  Alcotest.(check string)
    "nested template and trim" "left\027[1m\027[38;5;99mx\027[m\027[mright" rendered

let template_functions () =
  let sources =
    [
      "{{ Color \"99\" \"0\" \"x\" }}";
      "{{ Foreground \"99\" \"x\" }}";
      "{{ Background \"99\" \"x\" }}";
      "{{ Bold \"x\" }}";
      "{{ Faint \"x\" }}";
      "{{ Italic \"x\" }}";
      "{{ Underline \"x\" }}";
      "{{ Overline \"x\" }}";
      "{{ Blink \"x\" }}";
      "{{ Reverse \"x\" }}";
      "{{ CrossOut \"x\" }}";
    ]
  in
  List.iter
    (fun source ->
      match Format.render Format.Template source with
      | Ok output -> Alcotest.(check bool) source true (String.length output >= 1)
      | Error (`Msg message) -> Alcotest.failf "%s: %s" source message)
    sources

let all_renderers () =
  let markdown =
    expect_ok "markdown" (Format.render ~theme:"ascii" Format.Markdown "# Title")
  in
  let code =
    expect_ok "code"
      (Format.render ~theme:"ascii" ~language:"ocaml" Format.Code "let x = 1")
  in
  let emoji =
    expect_ok "emoji" (Format.render ~theme:"ascii" Format.Emoji "hello :heart:")
  in
  let plain value = Charamel_ansi.Text.strip value in
  Alcotest.(check bool)
    "markdown output" true
    (Test_support.contains ~needle:"# Title" ~haystack:(plain markdown));
  Alcotest.(check bool)
    "code output" true
    (Test_support.contains ~needle:"let x = 1" ~haystack:(plain code));
  Alcotest.(check bool)
    "emoji output" true
    (Test_support.contains ~needle:"hello ❤️" ~haystack:(plain emoji))

let invalid_template () =
  match Format.render Format.Template "{{ Missing \"x\" }}" with
  | Error (`Msg message) ->
      Alcotest.(check string)
        "unknown function diagnostic" "unknown template function \"Missing\"" message
  | Ok _ -> Alcotest.fail "unknown template function accepted"

let strip_input () =
  let rendered =
    expect_ok "strip"
      (Format.render ~strip_ansi:true Format.Template "\027[31mplain\027[0m")
  in
  Alcotest.(check string) "ANSI stripped before template evaluation" "plain" rendered

let cases =
  [
    Alcotest_lwt.test_case_sync "template styles" `Quick template_styles_text;
    Alcotest_lwt.test_case_sync "nested trim" `Quick nested_and_trim;
    Alcotest_lwt.test_case_sync "template functions" `Quick template_functions;
    Alcotest_lwt.test_case_sync "all renderers" `Quick all_renderers;
    Alcotest_lwt.test_case_sync "invalid template" `Quick invalid_template;
    Alcotest_lwt.test_case_sync "strip" `Quick strip_input;
  ]
