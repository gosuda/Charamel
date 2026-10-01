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
  let lines model =
    String.split_on_char '\n' (Charamel_ansi.Text.strip (BTable.view model))
  in
  let golden =
    [
      " Column     ";
      " ABCDEFGH           ";
      " longer-th…         ";
      "                    ";
      "                    ";
    ]
  in
  Alcotest.(check (list string)) "header rows and truncation marker" golden (lines model);
  Alcotest.(check (list string))
    "selection changes style only" golden
    (lines (BTable.move_down 1 model))

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

let paging_clamps () =
  let columns = [ { BTable.title = "Name"; width = 10 } ] in
  let rows = List.init 40 (fun index -> [ Fmt.str "row%d" index ]) in
  let model = BTable.v ~columns ~rows ~width:20 ~height:5 ~focused:true () in
  let go message model = fst (BTable.update message model) in
  let repeatedly message model count =
    List.fold_left (fun m _ -> go message m) model (List.init count Fun.id)
  in
  Alcotest.(check int) "starts at the first row" 0 (BTable.cursor model);
  let page = BTable.cursor (go BTable.Page_down model) in
  let half = BTable.cursor (go BTable.Half_page_down model) in
  Alcotest.(check bool) "a page moves down" true (page > 0);
  Alcotest.(check bool)
    "a half page moves less than a page" true
    (half > 0 && half <= page);
  Alcotest.(check int)
    "paging down clamps at the last row" 39
    (BTable.cursor (repeatedly BTable.Page_down model 20));
  Alcotest.(check (option (list string)))
    "the clamped selection is the last row" (Some [ "row39" ])
    (BTable.selected_row (repeatedly BTable.Page_down model 20));
  Alcotest.(check int)
    "paging up clamps at the first row" 0
    (BTable.cursor (repeatedly BTable.Page_up (repeatedly BTable.Page_down model 20) 20));
  Alcotest.(check int) "goto bottom" 39 (BTable.cursor (go BTable.Goto_bottom model));
  Alcotest.(check int)
    "goto top from the bottom" 0
    (BTable.cursor (go BTable.Goto_top (go BTable.Goto_bottom model)));
  Alcotest.(check int)
    "set_cursor clamps above the last row" 39
    (BTable.cursor (BTable.set_cursor 1000 model));
  Alcotest.(check int)
    "set_cursor clamps below the first row" 0
    (BTable.cursor (BTable.set_cursor (-5) model));
  Alcotest.(check int)
    "a blurred table ignores paging" 0
    (BTable.cursor (go BTable.Goto_bottom (BTable.blur model)))

let cases =
  [
    Alcotest_lwt.test_case_sync "values" `Quick values;
    Alcotest_lwt.test_case_sync "tab values" `Quick tab_values;
    Alcotest_lwt.test_case_sync "navigation" `Quick navigation;
    Alcotest_lwt.test_case_sync "rendering" `Quick rendering;
    Alcotest_lwt.test_case_sync "view cursor" `Quick view_cursor;
    Alcotest_lwt.test_case_sync "paging clamps" `Quick paging_clamps;
  ]
