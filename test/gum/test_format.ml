let expect_ok name = function
  | Ok value -> value
  | Error (`Msg message) -> Alcotest.failf "%s: %s" name message

let contains_sub text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop index =
    index + needle_length <= text_length
    && (String.sub text index needle_length = needle || loop (index + 1))
  in
  needle = "" || loop 0

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
  let plain value = Charm_ansi.Text.strip value in
  Alcotest.(check bool) "markdown output" true (contains_sub (plain markdown) "# Title");
  Alcotest.(check bool) "code output" true (contains_sub (plain code) "let x = 1");
  Alcotest.(check bool) "emoji output" true (contains_sub (plain emoji) "hello ❤️")

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
    Alcotest.test_case "template styles" `Quick template_styles_text;
    Alcotest.test_case "nested trim" `Quick nested_and_trim;
    Alcotest.test_case "template functions" `Quick template_functions;
    Alcotest.test_case "all renderers" `Quick all_renderers;
    Alcotest.test_case "invalid template" `Quick invalid_template;
    Alcotest.test_case "strip" `Quick strip_input;
  ]
