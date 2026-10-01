module Area = Charamel_bubbles.Textarea
module Key = Charamel_tea.Key

let focused area = fst (Area.focus area)
let update message area = fst (Area.update message area)
let set_cursor_col col area = Area.set_cursor_column col area

let multiline_and_unicode () =
  let area = Area.v ~width:20 ~height:3 () |> focused in
  let area = Area.insert_string "first\nsecond\n界" area in
  Alcotest.(check string) "newlines preserved" "first\nsecond\n界" (Area.value area);
  Alcotest.(check int) "logical lines" 3 (Area.line_count area);
  Alcotest.(check int) "cursor at final line" 2 (Area.line area);
  Alcotest.(check int) "cursor after CJK grapheme" 1 (Area.column area);
  let area = Area.cursor_up area in
  Alcotest.(check int) "cursor up" 1 (Area.line area);
  Alcotest.(check int) "cursor up clamps column" 2 (Area.column area)

let insertion_and_bounds () =
  let area = Area.v ~char_limit:7 ~max_content_height:4 () |> focused in
  let area = Area.insert_string "foo\nbar" area in
  Alcotest.(check string) "insert text" "foo\nbar" (Area.value area);
  let area = Area.insert_string "baz" area in
  Alcotest.(check string) "character limit includes newline" "foo\nbar" (Area.value area);
  let area = Area.set_char_limit 0 area |> Area.cursor_end in
  let area = Area.insert_string "\nquux" area in
  Alcotest.(check string) "newline paste" "foo\nbar\nquux" (Area.value area);
  let area = Area.set_max_content_height 2 area in
  let before = Area.value area in
  let area = Area.insert_string "\nblocked" area in
  Alcotest.(check string) "visual content cap" before (Area.value area)

let selection_and_replacement () =
  let area = Area.v ~width:20 ~height:3 () |> focused |> Area.set_value "one two" in
  let area = area |> Area.cursor_start |> set_cursor_col 0 in
  let area = update (Area.Key_action Area.Select_word_forward) area in
  Alcotest.(check bool) "word selection" true (Area.has_selection area);
  Alcotest.(check string) "selected text" "one" (Area.selected_text area);
  let area = update (Area.Insert "1") area in
  Alcotest.(check string) "typing replaces selection" "1 two" (Area.value area);
  let area = Area.select_all area in
  let area = update (Area.Key_action Area.Delete_character_backward) area in
  Alcotest.(check string) "backspace deletes selection" "" (Area.value area)

let pointer_selection () =
  let area =
    Area.v ~prompt:"" ~show_line_numbers:false ~width:20 ~height:2 ()
    |> focused |> Area.set_value "hello\nworld"
  in
  let area = Area.begin_selection ~x:0 ~y:0 area in
  let area = Area.extend_selection ~x:5 ~y:1 area |> Area.end_selection in
  Alcotest.(check string)
    "selection across lines" "hello\nworld" (Area.selected_text area);
  let area = Area.clear_selection area in
  Alcotest.(check bool) "clear selection" false (Area.has_selection area)

let view_and_real_cursor () =
  let area =
    Area.v ~prompt:"> " ~show_line_numbers:false ~width:8 ~height:2 ~placeholder:"hello"
      ()
    |> focused
  in
  let rendered = Charamel_ansi.Text.strip (Area.view area) in
  Alcotest.(check bool)
    "placeholder rendered" true
    (Test_support.contains ~needle:"hello" ~haystack:rendered);
  let area = Area.set_value "abcdefghijk" area in
  let info = Area.line_info area in
  Alcotest.(check bool) "wrapped width" true ((Area.line_info area).Area.width <= 6);
  Alcotest.(check bool) "visual height" true (info.Area.height >= 1);
  Alcotest.(check bool)
    "real cursor while focused" true
    (Option.is_some (Area.cursor area))

let end_of_line_visual_row () =
  let area =
    Area.v ~prompt:"" ~show_line_numbers:false ~width:20 ~height:3 ()
    |> focused |> Area.set_value "ab\ncd\nef"
  in
  let area = Area.cursor_up area in
  Alcotest.(check int) "cursor line" 1 (Area.line area);
  Alcotest.(check int) "cursor column" 2 (Area.column area);
  Alcotest.(check int)
    "cells before the line end" 2 (Area.line_info area).Area.char_offset;
  match Area.cursor area with
  | None -> Alcotest.fail "focused cursor request"
  | Some c -> Alcotest.(check int) "visual row of the line end" 1 c.row

let trailing_space_row_start () =
  let area =
    Area.v ~prompt:"" ~show_line_numbers:false ~width:5 ~height:5 ()
    |> focused |> Area.set_value "abcd efgh "
  in
  let info = Area.line_info area in
  Alcotest.(check int) "the wrapped space row keeps its start" 9 info.Area.start_column;
  Alcotest.(check int) "the cursor sits one cell into it" 1 info.Area.char_offset

let char_limit_counts_clusters () =
  let area = Area.v ~char_limit:3 () |> focused in
  let area = Area.insert_string "漢字漢" area in
  Alcotest.(check string) "a wide cluster costs one unit" "漢字漢" (Area.value area);
  let area = Area.insert_string "字" area in
  Alcotest.(check string) "the limit still binds" "漢字漢" (Area.value area)

let cjk_word_motion () =
  let area = Area.v ~width:20 ~height:3 () |> focused |> Area.set_value "日本語abc" in
  let back = update (Area.Key_action Area.Word_backward) area in
  Alcotest.(check int) "alt+left stops at the script boundary" 3 (Area.column back);
  let deleted = update (Area.Key_action Area.Delete_word_backward) back in
  Alcotest.(check string) "alt+backspace stops at the boundary" "abc" (Area.value deleted);
  let forward = update (Area.Key_action Area.Word_forward) (Area.cursor_start area) in
  Alcotest.(check int) "alt+right stops after the CJK run" 3 (Area.column forward);
  let deleted = update (Area.Key_action Area.Delete_word_forward) forward in
  Alcotest.(check string) "alt+delete stops at the boundary" "日本語" (Area.value deleted);
  let transformed = update (Area.Key_action Area.Uppercase_word_forward) forward in
  Alcotest.(check string)
    "the word case transform stops at the boundary" "日本語ABC" (Area.value transformed)

let samples n =
  let rand = Random.State.make [| 20260926 |] in
  List.init n (fun _ -> QCheck2.Gen.generate1 ~rand Test_support.cjk_gen)

let widest_cluster s =
  List.fold_left
    (fun n cluster -> max n (Charamel_ansi.Width.grapheme_width cluster))
    0
    (Charamel_ansi.Width.graphemes s)

let row_fits_box text width =
  let area =
    Area.v ~prompt:"" ~show_line_numbers:false ~virtual_cursor:false ~width ~height:4 ()
    |> focused |> Area.set_value text
  in
  let bound = max width (widest_cluster (Area.value area)) in
  let rows = String.split_on_char '\n' (Charamel_ansi.Text.strip (Area.view area)) in
  List.iter
    (fun row ->
      Alcotest.(check bool)
        (Format.sprintf "row %S exceeds width %d" row width)
        true
        (Charamel_ansi.Text.width row <= bound))
    rows

let generated_rows_fit_the_box () =
  List.iter
    (fun text -> List.iter (row_fits_box text) [ 2; 3; 5; 8; 13; 21 ])
    (samples 40)

let cursor_column_matches text =
  let area =
    Area.v ~prompt:"" ~show_line_numbers:false ~virtual_cursor:false ~width:200 ~height:4
      ()
    |> focused |> Area.set_value text
  in
  match Area.cursor area with
  | None -> Alcotest.fail "focused cursor request"
  | Some c ->
      let line =
        List.nth (String.split_on_char '\n' (Area.value area)) (Area.line area)
      in
      let before =
        String.concat ""
          (List.take (Area.column area) (Charamel_ansi.Width.graphemes line))
      in
      Alcotest.(check int)
        (Format.sprintf "cursor column for %S" text)
        (Charamel_ansi.Text.width before)
        c.col

let cursor_column_matches_rendered_cells () = List.iter cursor_column_matches (samples 40)

let wrap_bounds text width =
  let bound = max width (widest_cluster text) in
  Alcotest.(check bool)
    "truncate bound" true
    (Charamel_ansi.Text.width (Charamel_ansi.Text.truncate text ~width) <= bound);
  List.iter
    (fun row ->
      Alcotest.(check bool) "wrap bound" true (Charamel_ansi.Text.width row <= bound))
    (String.split_on_char '\n' (Charamel_ansi.Text.wrap text ~width))

let ansi_wrap_bounds () =
  List.iter (fun text -> List.iter (wrap_bounds text) [ 2; 5; 13 ]) (samples 40)

let keymap () =
  let area = Area.v () |> focused in
  match Area.key area (Key.v Key.Enter) with
  | Some (Area.Key_action Area.Insert_newline) -> ()
  | _ -> Alcotest.fail "enter binding"

let cases =
  [
    Alcotest_lwt.test_case_sync "multiline and unicode" `Quick multiline_and_unicode;
    Alcotest_lwt.test_case_sync "insertion and bounds" `Quick insertion_and_bounds;
    Alcotest_lwt.test_case_sync "selection and replacement" `Quick
      selection_and_replacement;
    Alcotest_lwt.test_case_sync "pointer selection" `Quick pointer_selection;
    Alcotest_lwt.test_case_sync "view and real cursor" `Quick view_and_real_cursor;
    Alcotest_lwt.test_case_sync "end of line visual row" `Quick end_of_line_visual_row;
    Alcotest_lwt.test_case_sync "trailing space row start" `Quick trailing_space_row_start;
    Alcotest_lwt.test_case_sync "character limit counts clusters" `Quick
      char_limit_counts_clusters;
    Alcotest_lwt.test_case_sync "CJK word motion" `Quick cjk_word_motion;
    Alcotest_lwt.test_case_sync "generated rows fit the box" `Quick
      generated_rows_fit_the_box;
    Alcotest_lwt.test_case_sync "cursor column matches rendered cells" `Quick
      cursor_column_matches_rendered_cells;
    Alcotest_lwt.test_case_sync "ansi wrap bounds" `Quick ansi_wrap_bounds;
    Alcotest_lwt.test_case_sync "keymap" `Quick keymap;
  ]
