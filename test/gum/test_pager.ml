let key = Test_gum_support.key

let check ~name ~needle frame =
  Alcotest.(check bool) name true (Test_support.contains ~needle ~haystack:frame)

let check_absent ~name ~needle frame =
  Alcotest.(check bool) name false (Test_support.contains ~needle ~haystack:frame)

let sanitize_backspaces () =
  Alcotest.(check string) "backspace overwrite" "red" (Pager.sanitize "redx\b")

let find_ranges () =
  Alcotest.(check (list (pair int int)))
    "matching ranges"
    [ (0, 3); (8, 11) ]
    (Pager.match_ranges ~pattern:"foo" "foo\nbar\nFOO")

let invalid_pattern_has_no_ranges () =
  Alcotest.(check (list (pair int int)))
    "unbalanced group" []
    (Pager.match_ranges ~pattern:"(" "anything")

let content = "l1 needle\nl2\nl3\nl4\nl5\nl6\nl7\nl8 needle\n"

let options =
  {
    Pager.default_options with
    content;
    show_line_numbers = true;
    soft_wrap = false;
    style = Gum_style.defaults ~padding:"0" ();
  }

let run events = snd (Charamel_tea.Test.run (Pager.app options) ~events ~size:(4, 40))

let gutter_numbers_render () =
  let frame = run [] in
  check ~name:"first line numbered" ~needle:"1 l1 needle" frame;
  check ~name:"fourth line numbered" ~needle:"4 l4" frame

let search_scrolls_to_each_match () =
  let accepted = [ `Key (key "/"); `Text "needle"; `Key (key "enter") ] in
  let frame = run accepted in
  check ~name:"first match visible" ~needle:"l1 needle" frame;
  check_absent ~name:"second match off screen" ~needle:"l8 needle" frame;
  let frame = run (accepted @ [ `Key (key "n") ]) in
  check ~name:"next match scrolled in" ~needle:"l8 needle" frame;
  check_absent ~name:"previous match scrolled out" ~needle:"l1 needle" frame

let cases =
  [
    Alcotest_lwt.test_case_sync "sanitize backspaces" `Quick sanitize_backspaces;
    Alcotest_lwt.test_case_sync "match ranges" `Quick find_ranges;
    Alcotest_lwt.test_case_sync "invalid pattern" `Quick invalid_pattern_has_no_ranges;
    Alcotest_lwt.test_case_sync "line-number gutter" `Quick gutter_numbers_render;
    Alcotest_lwt.test_case_sync "match scrolling" `Quick search_scrolls_to_each_match;
  ]
