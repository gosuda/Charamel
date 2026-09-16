module Area = Charm_bubbles.Textarea
module Key = Charm_tea.Key

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
  let rendered = Charm_ansi.Text.strip (Area.view area) in
  Alcotest.(check bool) "placeholder rendered" true (String.length rendered > 0);
  let area = Area.set_value "abcdefghijk" area in
  let info = Area.line_info area in
  Alcotest.(check bool) "wrapped width" true ((Area.line_info area).width <= 6);
  Alcotest.(check bool) "visual height" true (info.height >= 1);
  let area = Area.set_virtual_cursor false area in
  Alcotest.(check bool) "real cursor" true (Option.is_some (Area.cursor area));
  let area = Area.set_virtual_cursor true area in
  Alcotest.(check bool) "virtual cursor" true (Option.is_none (Area.cursor area))

let keymap () =
  let area = Area.v () |> focused in
  match Area.key area (Key.v Key.Enter) with
  | Some (Area.Key_action Area.Insert_newline) -> ()
  | _ -> Alcotest.fail "enter binding"

let cases =
  [
    Alcotest.test_case "multiline and unicode" `Quick multiline_and_unicode;
    Alcotest.test_case "insertion and bounds" `Quick insertion_and_bounds;
    Alcotest.test_case "selection and replacement" `Quick selection_and_replacement;
    Alcotest.test_case "pointer selection" `Quick pointer_selection;
    Alcotest.test_case "view and real cursor" `Quick view_and_real_cursor;
    Alcotest.test_case "keymap" `Quick keymap;
  ]
