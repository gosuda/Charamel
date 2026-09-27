let key = Test_gum_support.key

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

let label_cell frame label =
  let label_length = String.length label in
  let rec search row = function
    | [] -> Alcotest.failf "the frame shows no %S button" label
    | line :: rest ->
        let plain = Charamel_ansi.Text.strip line in
        let rec scan column =
          if column + label_length > String.length plain then search (row + 1) rest
          else if String.sub plain column label_length = label then
            (row, Charamel_ansi.Text.width (String.sub plain 0 column))
          else scan (column + 1)
        in
        scan 0
  in
  search 0 (String.split_on_char '\n' frame)

let caret_on_the_selected_button () =
  let options = { Confirm.default_options with show_help = false; padding = "0" } in
  let view default =
    (Confirm.app options).Charamel_tea.view (Confirm.make { options with default })
  in
  let caret frame =
    match frame.Charamel_tea.View.cursor with
    | None -> Alcotest.fail "the confirm view asks for a cursor"
    | Some (cursor : Charamel_tea.Cursor.t) ->
        (cursor.Charamel_tea.Cursor.row, cursor.Charamel_tea.Cursor.col)
  in
  let affirmative = view true in
  let negative = view false in
  Alcotest.(check (pair int int))
    "the caret marks the affirmative label"
    (label_cell affirmative.Charamel_tea.View.content "Yes")
    (caret affirmative);
  Alcotest.(check (pair int int))
    "the caret marks the negative label"
    (label_cell negative.Charamel_tea.View.content "No")
    (caret negative);
  let single =
    (Confirm.app { options with negative = "" }).Charamel_tea.view
      (Confirm.make { options with negative = "" })
  in
  Alcotest.(check (pair int int))
    "the caret marks the only button"
    (label_cell single.Charamel_tea.View.content "Yes")
    (caret single)

let cases =
  [
    Alcotest_lwt.test_case_sync "negative answer" `Quick negative_answer;
    Alcotest_lwt.test_case_sync "caret on the selected button" `Quick
      caret_on_the_selected_button;
    Alcotest_lwt.test_case_sync "toggle answer" `Quick toggle_answer;
  ]
