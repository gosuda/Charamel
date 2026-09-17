let wrapper_exports () =
  let key =
    match Charamel_tea.Key.of_string "q" with
    | Ok value -> value
    | Error (`Msg message) -> Alcotest.fail message
  in
  let binding = Charamel_bubbles.Key_binding.v [ "q" ] in
  Alcotest.(check bool)
    "key binding export" true
    (Charamel_bubbles.Key_binding.matches key binding);
  Alcotest.(check string) "duration export" "1s" (Charamel_bubbles.Duration.to_string 1.);
  Alcotest.(check (list int))
    "fuzzy export" [ 0 ]
    (Stdlib.List.map
       (fun (m : Charamel_bubbles.Fuzzy.match_) -> m.Charamel_bubbles.Fuzzy.index)
       (Charamel_bubbles.Fuzzy.find ~pattern:"a" [ "a" ]))

let cases = [ Alcotest.test_case "wrapper exports" `Quick wrapper_exports ]

let () =
  Alcotest.run "bubbles"
    [
      ("shared", cases);
      ("key binding", Test_key_binding.cases);
      ("fuzzy", Test_fuzzy.cases);
      ("duration", Test_duration.cases);
      ("cursor", Test_cursor.cases);
      ("textinput", Test_textinput.cases);
      ("textarea", Test_textarea.cases);
      ("viewport", Test_viewport.cases);
      ("list", Test_list.cases);
      ("table", Test_table.cases);
      ("spinner", Test_spinner.cases);
      ("progress", Test_progress.cases);
      ("paginator", Test_paginator.cases);
      ("help", Test_help.cases);
      ("timer", Test_timer.cases);
      ("stopwatch", Test_stopwatch.cases);
      ("filepicker", Test_filepicker.cases);
      ("tree", Test_tree.cases);
    ]
