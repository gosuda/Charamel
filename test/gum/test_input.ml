let key name =
  match Charamel_tea.Key.of_string name with
  | Ok key -> key
  | Error (`Msg message) -> Alcotest.fail message

let scripted_input () =
  let options = { Input.default_options with show_help = false; padding = "0" } in
  let model, _ =
    Charamel_tea.Test.run (Input.app options)
      ~events:[ `Text "hello"; `Key (key "enter") ]
      ~size:(8, 80)
  in
  Alcotest.(check string) "value" "hello" (Input.value model);
  Alcotest.(check bool) "submitted" true (Input.submitted model)

let password_mode () =
  let options =
    {
      Input.default_options with
      password = true;
      value = "secret";
      show_help = false;
      padding = "0";
    }
  in
  let model = Input.make options in
  let view = (Input.app options).Charamel_tea.view model in
  Alcotest.(check string) "password value retained" "secret" (Input.value model);
  Alcotest.(check bool)
    "masked frame differs" true
    (not (String.equal view.Charamel_tea.View.content "secret"))

let cases =
  [
    Alcotest_lwt.test_case_sync "scripted input" `Quick scripted_input;
    Alcotest_lwt.test_case_sync "password mode" `Quick password_mode;
  ]
