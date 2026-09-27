let key = Test_gum_support.key

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

let hardware_cursor_tracks_the_cjk_caret () =
  let options = { Input.default_options with show_help = false; padding = "0" } in
  let model, _ =
    Charamel_tea.Test.run (Input.app options) ~events:[ `Text "你好" ] ~size:(4, 40)
  in
  let view = (Input.app options).Charamel_tea.view model in
  match view.Charamel_tea.View.cursor with
  | None -> Alcotest.fail "the focused input asks for the hardware cursor"
  | Some (cursor : Charamel_tea.Cursor.t) ->
      Alcotest.(check int) "the caret sits on the input line" 0 cursor.row;
      (* "> " is two cells and 你好 is four, so the caret column is six. *)
      Alcotest.(check int)
        "the caret clears the prompt and the wide clusters" 6 cursor.col

let hide_mode_withholds_the_cursor () =
  let options =
    {
      Input.default_options with
      show_help = false;
      padding = "0";
      cursor_mode = Input.Hide;
    }
  in
  let model, _ =
    Charamel_tea.Test.run (Input.app options) ~events:[ `Text "你好" ] ~size:(4, 40)
  in
  let view = (Input.app options).Charamel_tea.view model in
  Alcotest.(check bool)
    "hide mode asks for no hardware cursor" true
    (view.Charamel_tea.View.cursor = None)

let cases =
  [
    Alcotest_lwt.test_case_sync "scripted input" `Quick scripted_input;
    Alcotest_lwt.test_case_sync "password mode" `Quick password_mode;
    Alcotest_lwt.test_case_sync "hardware cursor" `Quick
      hardware_cursor_tracks_the_cjk_caret;
    Alcotest_lwt.test_case_sync "hide cursor" `Quick hide_mode_withholds_the_cursor;
  ]
