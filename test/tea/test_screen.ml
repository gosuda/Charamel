module Screen = Charamel_tea__Screen
module View = Charamel_tea.View
module Cursor = Charamel_tea.Cursor

let sync body = "\x1b[?2026h" ^ body ^ "\x1b[?2026l"
let check_string name expected actual = Alcotest.(check string) name expected actual

let test_full_and_changed_frame () =
  let screen = Screen.create ~rows:3 ~cols:10 in
  let first = Screen.render screen (View.v "abc") in
  check_string "full frame" (sync "\x1b[>1u\x1b[?2004habc\x1b[K\x1b[?25l") first;
  let second = Screen.render screen (View.v "axc") in
  check_string "minimal changed run" (sync "\x1b[2Dxc") second;
  let shrunk = Screen.render screen (View.v "a") in
  check_string "inline shrink uses erase-to-end" (sync "\x1b[2D\x1b[K") shrunk;
  check_string "unchanged frame is empty" "" (Screen.render screen (View.v "a"))

let test_resize_repaints_old_lines () =
  let screen = Screen.create ~rows:3 ~cols:5 in
  ignore (Screen.render screen (View.v "one\ntwo"));
  Screen.resize screen ~rows:1 ~cols:5;
  let resized = Screen.render screen (View.v "one") in
  check_string "resize redraws and clears the line that disappeared"
    (sync "one\x1b[K\r\n\x1b[K\x1b[A\x1b[3C")
    resized

let test_wide_overwrite () =
  let screen = Screen.create ~rows:2 ~cols:4 in
  ignore (Screen.render screen (View.v "你a"));
  let output = Screen.render screen (View.v "x") in
  check_string "wide cell is overwritten through its continuation" (sync "\x1b[3Dx\x1b[K")
    output

let test_wide_backspace_replaces_leader () =
  let screen = Screen.create ~rows:2 ~cols:4 in
  check_string "backspace over a wide cell does not leave a continuation"
    (sync "\x1b[>1u\x1b[?2004h X\x1b[K\x1b[?25l")
    (Screen.render screen (View.v "你\bX"))

let test_style_and_link_cells () =
  let screen = Screen.create ~rows:2 ~cols:20 in
  let content = "\x1b[31m\x1b]8;;https://example.test\x07a\x1b]8;;\x07" in
  let output = Screen.render screen (View.v content) in
  check_string "SGR and OSC 8 are emitted in cell order"
    (sync "\x1b[>1u\x1b[?2004h\x1b]8;;https://example.test\x07\x1b[31ma\x1b[K\x1b[?25l")
    output;
  check_string "equal styled linked cells are stable" ""
    (Screen.render screen (View.v content))

let test_mode_only_delta () =
  let screen = Screen.create ~rows:2 ~cols:10 in
  ignore (Screen.render screen (View.v "a"));
  let keyboard =
    {
      View.disambiguate = true;
      report_events = false;
      report_alternates = false;
      report_all_keys = false;
      report_text = false;
    }
  in
  let view =
    View.v ~mouse:View.Mouse_click ~bracketed_paste:false ~report_focus:true ~title:"tea"
      ~keyboard "a"
  in
  check_string "mode-only frame has no repaint"
    (sync "\x1b[?1000h\x1b[?1006h\x1b[?2004l\x1b[?1004h\x1b]2;tea\x07")
    (Screen.render screen view);
  check_string "mode changes are reversed without repaint"
    (sync "\x1b[?1000l\x1b[?1006l\x1b[?2004h\x1b[?1004l\x1b]2;\x07")
    (Screen.render screen (View.v "a"))

let test_cursor_colors_and_progress () =
  let screen = Screen.create ~rows:2 ~cols:10 in
  ignore (Screen.render screen (View.v "x"));
  let cursor = { Cursor.row = 0; col = 1; shape = Cursor.Underline; blink = false } in
  let view =
    View.v ~cursor
      ~background:(Charamel_ansi.Color.Rgb (1, 2, 3))
      ~foreground:(Charamel_ansi.Color.Rgb (4, 5, 6))
      ~progress:(View.Progress_value 150) "x"
  in
  check_string "cursor colors and progress are mode-only"
    (sync "\x1b]11;#010203\x07\x1b]10;#040506\x07\x1b]9;4;1;100\x07\x1b[?25h\x1b[4 q")
    (Screen.render screen view);
  check_string "restore resets cursor colors and progress"
    "\x1b[?2026l\x1b[?2004l\x1b[0 q\x1b]111\x07\x1b]110\x07\x1b]9;4;0\x07\x1b[m\x1b[<u"
    (Screen.restore screen)

let test_kitty_alt_screen_stacks () =
  let screen = Screen.create ~rows:2 ~cols:8 in
  let keyboard =
    {
      View.disambiguate = true;
      report_events = true;
      report_alternates = false;
      report_all_keys = false;
      report_text = false;
    }
  in
  let alt = View.v ~alt_screen:true ~keyboard "a" in
  check_string "alternate screen pushes its keyboard stack"
    (sync "\x1b[?1049h\x1b[>3u\x1b[?2004h\x1b[2J\x1b[Ha\x1b[?25l")
    (Screen.render screen alt);
  let inline = View.v ~keyboard "a" in
  check_string "leaving alternate screen swaps stacks"
    (sync "\x1b[<u\x1b[?1049l\x1b[>3ua\x1b[K")
    (Screen.render screen inline)

let test_clear_reset_restore () =
  let screen = Screen.create ~rows:3 ~cols:5 in
  ignore (Screen.render screen (View.v "one\ntwo"));
  check_string "clear returns to the inline anchor"
    "\x1b[A\x1b[3D\x1b[2K\n\x1b[2K\x1b[A\r" (Screen.clear screen);
  check_string "clear resets only grid ownership" (sync "one\x1b[K")
    (Screen.render screen (View.v "one"));
  Screen.reset screen;
  check_string "reset makes the next frame fresh"
    (sync "\x1b[>1u\x1b[?2004hone\x1b[K\x1b[?25l")
    (Screen.render screen (View.v "one"));
  check_string "restore releases terminal modes"
    "\x1b[?2026l\x1b[?2004l\x1b[?25h\x1b[m\x1b[<u" (Screen.restore screen)

let cases =
  [
    Alcotest.test_case "full and changed frames" `Quick test_full_and_changed_frame;
    Alcotest.test_case "resize" `Quick test_resize_repaints_old_lines;
    Alcotest.test_case "wide overwrite" `Quick test_wide_overwrite;
    Alcotest.test_case "wide backspace replaces leader" `Quick
      test_wide_backspace_replaces_leader;
    Alcotest.test_case "styles and links" `Quick test_style_and_link_cells;
    Alcotest.test_case "mode-only delta" `Quick test_mode_only_delta;
    Alcotest.test_case "cursor colors and progress" `Quick test_cursor_colors_and_progress;
    Alcotest.test_case "kitty alternate stacks" `Quick test_kitty_alt_screen_stacks;
    Alcotest.test_case "clear reset restore" `Quick test_clear_reset_restore;
  ]
