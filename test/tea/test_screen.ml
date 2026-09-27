module Screen = Charamel_tea__Screen
module View = Charamel_tea.View
module Cursor = Charamel_tea.Cursor

let sync body = "\x1b[?2026h" ^ body ^ "\x1b[?2026l"

let test_full_and_changed_frame () =
  let screen = Screen.create ~rows:3 ~cols:10 in
  let first = Screen.render screen (View.v "abc") in
  Alcotest.(check string)
    "full frame"
    (sync "\x1b[>1u\x1b[?2004habc\x1b[K\x1b[?25l")
    first;
  let second = Screen.render screen (View.v "axc") in
  Alcotest.(check string) "minimal changed run" (sync "\x1b[2Dxc") second;
  let shrunk = Screen.render screen (View.v "a") in
  Alcotest.(check string) "inline shrink uses erase-to-end" (sync "\x1b[2D\x1b[K") shrunk;
  Alcotest.(check string)
    "unchanged frame is empty" ""
    (Screen.render screen (View.v "a"))

let test_resize_repaints_old_lines () =
  let screen = Screen.create ~rows:3 ~cols:5 in
  ignore (Screen.render screen (View.v "one\ntwo"));
  Screen.resize screen ~rows:1 ~cols:5;
  let resized = Screen.render screen (View.v "one") in
  Alcotest.(check string)
    "resize redraws and clears the line that disappeared"
    (sync "one\x1b[K\r\n\x1b[K\x1b[A\x1b[3C")
    resized

let test_wide_overwrite () =
  let screen = Screen.create ~rows:2 ~cols:4 in
  ignore (Screen.render screen (View.v "你a"));
  let output = Screen.render screen (View.v "x") in
  Alcotest.(check string)
    "wide cell is overwritten through its continuation" (sync "\x1b[3Dx\x1b[K") output

let test_wide_backspace_replaces_leader () =
  let screen = Screen.create ~rows:2 ~cols:4 in
  Alcotest.(check string)
    "backspace over a wide cell does not leave a continuation"
    (sync "\x1b[>1u\x1b[?2004h X\x1b[K\x1b[?25l")
    (Screen.render screen (View.v "你\bX"))

let test_style_and_link_cells () =
  let screen = Screen.create ~rows:2 ~cols:20 in
  let content = "\x1b[31m\x1b]8;;https://example.test\x07a\x1b]8;;\x07" in
  let output = Screen.render screen (View.v content) in
  Alcotest.(check string)
    "SGR and OSC 8 are emitted in cell order"
    (sync "\x1b[>1u\x1b[?2004h\x1b]8;;https://example.test\x07\x1b[31ma\x1b[K\x1b[?25l")
    output;
  Alcotest.(check string)
    "equal styled linked cells are stable" ""
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
  Alcotest.(check string)
    "mode-only frame has no repaint"
    (sync "\x1b[?1000h\x1b[?1006h\x1b[?2004l\x1b[?1004h\x1b]2;tea\x07")
    (Screen.render screen view);
  Alcotest.(check string)
    "mode changes are reversed without repaint"
    (sync "\x1b[?1000l\x1b[?1006l\x1b[?2004h\x1b[?1004l\x1b]2;\x07")
    (Screen.render screen (View.v "a"))

let test_cursor_colors_and_progress () =
  let screen = Screen.create ~rows:2 ~cols:10 in
  ignore (Screen.render screen (View.v "x"));
  let cursor = Cursor.v ~shape:Cursor.Underline ~blink:false 0 1 in
  let view =
    View.v ~cursor
      ~background:(Charamel_ansi.Color.Rgb (1, 2, 3))
      ~foreground:(Charamel_ansi.Color.Rgb (4, 5, 6))
      ~progress:(View.Progress_value 150) "x"
  in
  Alcotest.(check string)
    "cursor colors and progress are mode-only"
    (sync "\x1b]11;#010203\x07\x1b]10;#040506\x07\x1b]9;4;1;100\x07\x1b[?25h\x1b[4 q")
    (Screen.render screen view);
  Alcotest.(check string)
    "restore resets cursor colors and progress"
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
  Alcotest.(check string)
    "alternate screen pushes its keyboard stack"
    (sync "\x1b[?1049h\x1b[>3u\x1b[?2004h\x1b[2J\x1b[Ha\x1b[?25l")
    (Screen.render screen alt);
  let inline = View.v ~keyboard "a" in
  Alcotest.(check string)
    "leaving alternate screen swaps stacks"
    (sync "\x1b[<u\x1b[?1049l\x1b[>3ua\x1b[K")
    (Screen.render screen inline)

let test_clear_reset_restore () =
  let screen = Screen.create ~rows:3 ~cols:5 in
  ignore (Screen.render screen (View.v "one\ntwo"));
  Alcotest.(check string)
    "clear returns to the inline anchor" "\x1b[A\x1b[3D\x1b[2K\n\x1b[2K\x1b[A\r"
    (Screen.clear screen);
  Alcotest.(check string)
    "clear resets only grid ownership" (sync "one\x1b[K")
    (Screen.render screen (View.v "one"));
  Screen.reset screen;
  Alcotest.(check string)
    "reset makes the next frame fresh"
    (sync "\x1b[>1u\x1b[?2004hone\x1b[K\x1b[?25l")
    (Screen.render screen (View.v "one"));
  Alcotest.(check string)
    "restore releases terminal modes" "\x1b[?2026l\x1b[?2004l\x1b[?25h\x1b[m\x1b[<u"
    (Screen.restore screen)

let test_cursor_color_diff () =
  let screen = Screen.create ~rows:2 ~cols:10 in
  ignore (Screen.render screen (View.v "x"));
  let rgb = Charamel_ansi.Color.Rgb (1, 2, 3) in
  let colored = Screen.render screen (View.v ~cursor:(Cursor.v ~color:rgb 0 1) "x") in
  Alcotest.(check string)
    "OSC 12 carries the cursor color"
    (sync "\x1b[?25h\x1b]12;#010203\x1b\\")
    colored;
  Alcotest.(check string)
    "an unchanged cursor color is silent" ""
    (Screen.render screen (View.v ~cursor:(Cursor.v ~color:rgb 0 1) "x"));
  Alcotest.(check string)
    "leaving the color resets with OSC 112" (sync "\x1b]112\x1b\\")
    (Screen.render screen (View.v ~cursor:(Cursor.v 0 1) "x"));
  ignore (Screen.render screen (View.v ~cursor:(Cursor.v ~color:rgb 0 1) "x"));
  Alcotest.(check string)
    "restore resets an applied cursor color"
    "\x1b[?2026l\x1b[?2004l\x1b]112\x1b\\\x1b[m\x1b[<u" (Screen.restore screen)

let test_cursor_snaps_off_wide_continuation () =
  let screen = Screen.create ~rows:2 ~cols:6 in
  Alcotest.(check string)
    "a cursor on a continuation cell lands on the glyph"
    (sync "\x1b[?1049h\x1b[>1u\x1b[?2004h\x1b[2J\x1b[H漢x\x1b[H")
    (Screen.render screen (View.v ~alt_screen:true ~cursor:(Cursor.v 0 1) "漢x"))

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
    Alcotest.test_case "cursor color diff" `Quick test_cursor_color_diff;
    Alcotest.test_case "cursor snaps off wide continuation" `Quick
      test_cursor_snaps_off_wide_continuation;
  ]
