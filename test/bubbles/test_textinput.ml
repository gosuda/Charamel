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
    "single line paste sanitizes newline" "x yabcdef" (Input.value input);
  let input = Input.set_virtual_cursor false input in
  Alcotest.(check bool) "real cursor request" true (Option.is_some (Input.cursor input))

let keymap () =
  let input = Input.v () |> focused in
  let key = Key.v Key.Left in
  match Input.key input key with
  | Some Input.Character_backward -> ()
  | _ -> Alcotest.fail "left binding"

let cases =
  [
    Alcotest.test_case "editing unicode and limits" `Quick editing_unicode_and_limits;
    Alcotest.test_case "validation" `Quick validation;
    Alcotest.test_case "suggestions" `Quick suggestions;
    Alcotest.test_case "scrolling and paste" `Quick scrolling_and_paste;
    Alcotest.test_case "keymap" `Quick keymap;
  ]
