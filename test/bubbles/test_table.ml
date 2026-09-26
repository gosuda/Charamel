module BTable = Charamel_bubbles.Table

let values () =
  let rows = BTable.of_values "foo1,bar1\nfoo2,bar2\nfoo3,bar3" in
  Alcotest.(check int) "row count" 3 (List.length rows);
  Alcotest.(check (list string)) "first row" [ "foo1"; "bar1" ] (List.hd rows)

let tab_values () =
  let rows = BTable.of_values ~separator:"\t" "a\tb\nc\td" in
  Alcotest.(check (list string)) "tab row" [ "a"; "b" ] (List.hd rows)

let navigation () =
  let columns = [ { BTable.title = "Name"; width = 10 } ] in
  let rows = [ [ "one" ]; [ "two" ]; [ "three" ]; [ "four" ] ] in
  let model = BTable.v ~columns ~rows ~width:20 ~height:5 ~focused:true () in
  let model = BTable.move_down 2 model in
  Alcotest.(check int) "move down" 2 (BTable.cursor model);
  let model = BTable.move_down 5 model in
  Alcotest.(check int) "clamped down" 3 (BTable.cursor model);
  let model = BTable.move_up 10 model in
  Alcotest.(check int) "clamped up" 0 (BTable.cursor model);
  let model = BTable.blur model in
  Alcotest.(check bool) "blurred" false (BTable.focused model);
  Alcotest.(check bool)
    "blurred key ignored" true
    (BTable.key model (Charamel_tea.Key.v Charamel_tea.Key.Down) = None)

let rendering () =
  let columns = [ { BTable.title = "Column"; width = 10 } ] in
  let rows = [ [ "ABCDEFGH" ]; [ "longer-than-ten" ] ] in
  let model = BTable.v ~columns ~rows ~width:20 ~height:5 ~focused:true () in
  let plain = Charamel_ansi.Text.strip (BTable.view model) in
  let lines = String.split_on_char '\n' plain in
  Alcotest.(check bool) "header and rows" true (List.length lines >= 3);
  Alcotest.(check bool) "first value" true (String.contains plain 'A');
  Alcotest.(check bool) "truncation marker" true (String.contains plain '\226');
  let model = BTable.move_down 1 model in
  let plain = Charamel_ansi.Text.strip (BTable.view model) in
  Alcotest.(check bool) "second value" true (String.contains plain 'l')

let cursor_row (cursor : Charamel_tea.Cursor.t option) =
  match cursor with None -> -1 | Some cursor -> cursor.row

let cursor_col (cursor : Charamel_tea.Cursor.t option) =
  match cursor with None -> -1 | Some cursor -> cursor.col

let view_cursor () =
  let columns = [ { BTable.title = "Name"; width = 10 } ] in
  let rows = List.init 40 (fun index -> [ Fmt.str "row%d" index ]) in
  let model = BTable.v ~columns ~rows ~width:20 ~height:5 ~focused:true () in
  Alcotest.(check int)
    "first row sits under the header" 1
    (cursor_row (BTable.view_cursor model));
  Alcotest.(check int)
    "the column is the row start" 0
    (cursor_col (BTable.view_cursor model));
  let scrolled = BTable.move_down 30 model in
  let row = cursor_row (BTable.view_cursor scrolled) in
  Alcotest.(check bool)
    "the cursor stays on the selected row inside the window" true
    (row >= 1 && row <= 4);
  let blurred = BTable.blur scrolled in
  Alcotest.(check bool)
    "a blurred table asks for no cursor" true
    (BTable.view_cursor blurred = None);
  let empty = BTable.v ~width:20 ~focused:true () in
  Alcotest.(check bool)
    "an empty table asks for no cursor" true
    (BTable.view_cursor empty = None)

let cases =
  [
    Alcotest_lwt.test_case_sync "values" `Quick values;
    Alcotest_lwt.test_case_sync "tab values" `Quick tab_values;
    Alcotest_lwt.test_case_sync "navigation" `Quick navigation;
    Alcotest_lwt.test_case_sync "rendering" `Quick rendering;
    Alcotest_lwt.test_case_sync "view cursor" `Quick view_cursor;
  ]
