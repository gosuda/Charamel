let key name =
  match Charamel_tea.Key.of_string name with
  | Ok key -> key
  | Error (`Msg message) -> Alcotest.fail message

let parse_options () =
  match Choose.parse_options ~delimiter:":" [ "one:1"; "two:2" ] with
  | Error message -> Alcotest.fail message
  | Ok items -> Alcotest.(check int) "two options" 2 (List.length items)

let scripted_selection () =
  let options =
    {
      Choose.default_options with
      options = [ "alpha"; "beta" ];
      show_help = false;
      padding = "0";
    }
  in
  let model, _frame =
    Charamel_tea.Test.run (Choose.app options)
      ~events:[ `Key (key "down"); `Key (key "enter") ]
      ~size:(12, 80)
  in
  Alcotest.(check (list string)) "selected option" [ "beta" ] (Choose.selected model);
  Alcotest.(check bool) "submitted" true (Choose.submitted model)

let non_tty_shortcut () =
  let options =
    { Choose.default_options with options = [ "only" ]; select_if_one = true }
  in
  match Choose.single_option options with
  | Ok (Some value) -> Alcotest.(check string) "sole value" "only" value
  | Ok None -> Alcotest.fail "shortcut was not selected"
  | Error message -> Alcotest.fail message

let cases =
  [
    Alcotest.test_case "option parsing" `Quick parse_options;
    Alcotest.test_case "scripted selection" `Quick scripted_selection;
    Alcotest.test_case "non-tty sole option" `Quick non_tty_shortcut;
  ]
