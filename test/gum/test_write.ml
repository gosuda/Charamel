let key name =
  match Charamel_tea.Key.of_string name with
  | Ok key -> key
  | Error (`Msg message) -> Alcotest.fail message

let multiline_input () =
  let options = { Write.default_options with show_help = false; padding = "0" } in
  let model, _ =
    Charamel_tea.Test.run (Write.app options)
      ~events:[ `Text "hello"; `Key (key "ctrl+j"); `Text "world"; `Key (key "enter") ]
      ~size:(12, 80)
  in
  Alcotest.(check string) "multiline value" "hello\nworld" (Write.value model);
  Alcotest.(check bool) "submitted" true (Write.submitted model)

let line_limit () =
  Alcotest.(check string)
    "keeps first lines" "one\ntwo"
    (Write.normalize_lines ~max_lines:2 "one\ntwo\nthree")

let cases =
  [
    Alcotest.test_case "multiline input" `Quick multiline_input;
    Alcotest.test_case "line limit" `Quick line_limit;
  ]
