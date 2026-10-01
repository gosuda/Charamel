module Cursor = Charamel_bubbles.Cursor

let check_view expected cursor =
  Alcotest.(check string)
    "cursor text" expected
    (Charamel_ansi.Text.strip (Cursor.view cursor))

let blink_and_focus () =
  let cursor = Cursor.v () |> Cursor.set_char "x" in
  Alcotest.(check bool) "unfocused starts hidden" true (Cursor.is_blinked cursor);
  check_view "x" cursor;
  let cursor = Cursor.focus cursor in
  Alcotest.(check bool) "focus shows" false (Cursor.is_blinked cursor);
  let cursor = Cursor.update Cursor.Tick cursor in
  Alcotest.(check bool) "first tick toggles" true (Cursor.is_blinked cursor);
  let cursor = Cursor.show cursor in
  Alcotest.(check bool) "show is immediate" false (Cursor.is_blinked cursor);
  let cursor = Cursor.update Cursor.Tick cursor in
  Alcotest.(check bool) "show absorbs next tick" false (Cursor.is_blinked cursor)

let modes () =
  let cursor = Cursor.v () |> Cursor.set_char "x" |> Cursor.focus in
  let cursor = Cursor.set_mode Cursor.Static cursor in
  Alcotest.(check bool) "static stays visible" false (Cursor.is_blinked cursor);
  let cursor = Cursor.update Cursor.Tick cursor in
  Alcotest.(check bool) "static ignores ticks" false (Cursor.is_blinked cursor);
  let cursor = Cursor.set_mode Cursor.Hide cursor in
  Alcotest.(check bool) "hide" true (Cursor.is_blinked cursor);
  let cursor = Cursor.focus cursor in
  Alcotest.(check bool) "hide remains hidden" true (Cursor.is_blinked cursor);
  let cursor = Cursor.blur cursor in
  Alcotest.(check bool) "blur" false (Cursor.focused cursor)

let cases =
  [
    Alcotest_lwt.test_case_sync "blink and focus" `Quick blink_and_focus;
    Alcotest_lwt.test_case_sync "modes" `Quick modes;
  ]
