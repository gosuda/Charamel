let key name =
  match Charm_tea.Key.of_string name with
  | Ok key -> key
  | Error (`Msg message) -> Alcotest.fail message

let scripted_input () =
  let options = { Input.default_options with show_help = false; padding = "0" } in
  let model, _ =
    Charm_tea.Test.run (Input.app options)
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
  let view = (Input.app options).view model in
  Alcotest.(check string) "password value retained" "secret" (Input.value model);
  Alcotest.(check bool)
    "masked frame differs" true
    (not (String.equal view.content "secret"))

let cases =
  [
    Alcotest.test_case "scripted input" `Quick scripted_input;
    Alcotest.test_case "password mode" `Quick password_mode;
  ]
