let key name =
  match Charamel_tea.Key.of_string name with
  | Ok key -> key
  | Error (`Msg message) -> Alcotest.fail message

let negative_answer () =
  let options = { Confirm.default_options with show_help = false; padding = "0" } in
  let model, _frame =
    Charamel_tea.Test.run (Confirm.app options) ~events:[ `Key (key "n") ] ~size:(8, 80)
  in
  Alcotest.(check bool) "negative" false (Confirm.answer model);
  Alcotest.(check bool) "submitted" true (Confirm.submitted model)

let toggle_answer () =
  let options = { Confirm.default_options with show_help = false; padding = "0" } in
  let model, _ =
    Charamel_tea.Test.run (Confirm.app options)
      ~events:[ `Key (key "left"); `Key (key "enter") ]
      ~size:(8, 80)
  in
  Alcotest.(check bool) "toggled affirmative" false (Confirm.answer model)

let cases =
  [
    Alcotest_lwt.test_case_sync "negative answer" `Quick negative_answer;
    Alcotest_lwt.test_case_sync "toggle answer" `Quick toggle_answer;
  ]
