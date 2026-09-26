module Input = Charamel_bubbles.Textinput
module Key = Charamel_tea.Key

let focused input = fst (Input.focus input)
let update message input = fst (Input.update message input)

let editing_unicode_and_limits () =
  let input = Input.v ~char_limit:2 () |> focused in
  let input = update (Input.Insert "a\xCC\x81") input in
  Alcotest.(check string) "combining cluster is one value" "a\xCC\x81" (Input.value input);
  Alcotest.(check int) "cluster cursor" 1 (Input.position input);
  let input = update (Input.Insert "界b") input in
  Alcotest.(check string) "character limit" "a\xCC\x81界" (Input.value input);
  let input = update Input.Character_backward input in
  Alcotest.(check int) "backward by grapheme" 1 (Input.position input);
  let input = update Input.Delete_character_forward input in
  Alcotest.(check string) "delete grapheme" "a\xCC\x81" (Input.value input)

let validation () =
  let validate s = if s = "ok" then Ok () else Error "not ok" in
  let input = Input.v ~validate () |> focused |> fun m -> update (Input.Insert "no") m in
  Alcotest.(check (option string)) "validation error" (Some "not ok") (Input.error input);
  let input = update Input.Delete_character_backward input in
  Alcotest.(check string) "editing keeps value" "n" (Input.value input);
  let input = Input.set_value "ok" input in
  Alcotest.(check (option string)) "valid value clears error" None (Input.error input)

let suggestions () =
  let input =
    Input.v ~show_suggestions:true ~suggestions:[ "test1"; "test2"; "test3" ] ()
    |> focused
  in
  let input = Input.set_value "test" input in
  Alcotest.(check (list string))
    "prefix matches" [ "test1"; "test2"; "test3" ]
    (Input.matched_suggestions input);
  let input = update Input.Next_suggestion input in
  Alcotest.(check string) "next suggestion" "test2" (Input.current_suggestion input);
  let input = update Input.Accept_suggestion input in
  Alcotest.(check string) "accept suggestion" "test2" (Input.value input);
  let input = Input.blur input in
  Alcotest.(check bool)
    "blur hides completion" true
    (not (String.contains (Input.view input) '1'))

let scrolling_and_paste () =
  let input = Input.v ~width:4 () |> focused |> Input.set_value "abcdef" in
  let view = Charamel_ansi.Text.strip (Input.view input) in
  Alcotest.(check bool) "right edge remains visible" true (String.contains view 'f');
  let input = Input.cursor_start input in
  let view = Charamel_ansi.Text.strip (Input.view input) in
  Alcotest.(check bool) "left edge remains visible" true (String.contains view 'a');
  let input = Input.paste "x\ny" input in
  Alcotest.(check string)
    "single line paste sanitizes newline" "x yabcdef" (Input.value input)

let real_cursor_while_focused () =
  let input = Input.v () |> focused in
  Alcotest.(check bool)
    "cursor with the embedded cursor drawn" true
    (Option.is_some (Input.cursor input));
  let input = Input.set_virtual_cursor false input in
  Alcotest.(check bool)
    "cursor with the embedded cursor off" true
    (Option.is_some (Input.cursor input));
  Alcotest.(check bool)
    "blurred reports no cursor" true
    (Option.is_none (Input.cursor (Input.blur input)))

let folded_suggestion_prefix () =
  let input = Input.v ~show_suggestions:true ~suggestions:[ "ab"; "abc" ] ~value:"İ" () in
  Alcotest.(check (list string))
    "a fold expansion does not slice past the candidate" []
    (Input.matched_suggestions input);
  let input =
    Input.v ~show_suggestions:true ~suggestions:[ "İ"; "xy" ] ~value:"i\xCC\x87" ()
  in
  Alcotest.(check (list string))
    "a fold shrink still matches the prefix" [ "İ" ]
    (Input.matched_suggestions input);
  let input = Input.set_value "ǅ" input in
  Alcotest.(check (list string))
    "the update path compares folded lengths too" []
    (Input.matched_suggestions input)

let echo_mask_covers_each_cell () =
  let input =
    Input.v ~prompt:"" ~echo:Input.Password ~echo_character:"漢字" ~width:8
      ~virtual_cursor:false ()
    |> focused |> Input.set_value "abcd"
  in
  let rendered = Charamel_ansi.Text.strip (Input.view input) in
  Alcotest.(check string) "one mask cluster per cell" "漢漢漢漢" rendered;
  Alcotest.(check int)
    "the window holds the displayed width" 8
    (Charamel_ansi.Text.width rendered);
  let input = Input.set_echo_character "ab" input in
  Alcotest.(check string)
    "the setter clamps to one cluster" "aaaa"
    (String.sub (Charamel_ansi.Text.strip (Input.view input)) 0 4)

let cjk_word_motion () =
  let input = Input.v () |> focused |> Input.set_value "日本語abc" in
  let input = update Input.Word_backward input in
  Alcotest.(check int) "alt+left stops at the script boundary" 3 (Input.position input);
  let input = update Input.Delete_word_backward input in
  Alcotest.(check string) "alt+backspace stops at the boundary" "abc" (Input.value input);
  let input = Input.v () |> focused |> Input.set_value "日本語 abc" |> Input.cursor_start in
  let input = update Input.Word_forward input in
  Alcotest.(check int) "alt+right stops after the CJK run" 3 (Input.position input);
  let input = update Input.Word_forward input in
  Alcotest.(check int) "the next stop is the end" 7 (Input.position input)

let keymap () =
  let input = Input.v () |> focused in
  let key = Key.v Key.Left in
  match Input.key input key with
  | Some Input.Character_backward -> ()
  | _ -> Alcotest.fail "left binding"

let cases =
  [
    Alcotest_lwt.test_case_sync "editing unicode and limits" `Quick
      editing_unicode_and_limits;
    Alcotest_lwt.test_case_sync "validation" `Quick validation;
    Alcotest_lwt.test_case_sync "suggestions" `Quick suggestions;
    Alcotest_lwt.test_case_sync "scrolling and paste" `Quick scrolling_and_paste;
    Alcotest_lwt.test_case_sync "real cursor while focused" `Quick
      real_cursor_while_focused;
    Alcotest_lwt.test_case_sync "folded suggestion prefix" `Quick folded_suggestion_prefix;
    Alcotest_lwt.test_case_sync "echo mask covers each cell" `Quick
      echo_mask_covers_each_cell;
    Alcotest_lwt.test_case_sync "CJK word motion" `Quick cjk_word_motion;
    Alcotest_lwt.test_case_sync "keymap" `Quick keymap;
  ]
