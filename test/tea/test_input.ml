module Input = Charm_tea__Input
module Key = Charm_tea.Key
module Event = Charm_tea.Event

let decode input =
  let decoder = Input.create () in
  Input.feed decoder input @ Input.flush decoder

let only_key = function
  | [ Event.Key key ] -> key
  | events -> Alcotest.failf "expected one key event, got %d" (List.length events)

let test_plain_and_controls () =
  let events = decode "aA \t\r\000\001\008\031\127" in
  match events with
  | [
   Event.Key a;
   Event.Key upper;
   Event.Key space;
   Event.Key tab;
   Event.Key enter;
   Event.Key ctrl_space;
   Event.Key ctrl_a;
   Event.Key ctrl_h;
   Event.Key ctrl_us;
   Event.Key backspace;
  ] ->
      Alcotest.(check string) "a" "a" (Key.to_string a);
      Alcotest.(check string) "uppercase" "shift+a" (Key.to_string upper);
      Alcotest.(check string) "space" "space" (Key.to_string space);
      Alcotest.(check string) "tab" "tab" (Key.to_string tab);
      Alcotest.(check string) "enter" "enter" (Key.to_string enter);
      Alcotest.(check string) "ctrl-space" "ctrl+space" (Key.to_string ctrl_space);
      Alcotest.(check string) "ctrl-a" "ctrl+a" (Key.to_string ctrl_a);
      Alcotest.(check string) "ctrl-h" "ctrl+h" (Key.to_string ctrl_h);
      Alcotest.(check string) "ctrl-underscore" "ctrl+_" (Key.to_string ctrl_us);
      Alcotest.(check string) "backspace" "backspace" (Key.to_string backspace)
  | _ -> Alcotest.failf "unexpected control event count: %d" (List.length events)

let test_legacy_csi_ss3_and_urxvt () =
  let input = "\027[A\027[1;5C\027[15~\027OP\027Ob\027[1$" in
  let events = decode input in
  let names =
    List.map
      (function
        | Event.Key key -> Key.to_string key
        | Event.Unknown raw -> Fmt.str "unknown:%S" raw
        | _ -> "other")
      events
  in
  Alcotest.(check (list string))
    "legacy names"
    [ "up"; "ctrl+right"; "f5"; "f1"; "ctrl+down"; "shift+home" ]
    names

let test_legacy_modifiers_and_kitty_extensions () =
  let events = decode "\027[1;4:2B\027[1;1:2B" in
  match events with
  | [ Event.Key first; Event.Key second ] ->
      Alcotest.(check string) "colon modifier" "alt+shift+down" (Key.to_string first);
      Alcotest.(check string) "kitty repeat" "down" (Key.to_string second);
      Alcotest.(check bool) "repeat event" true (second.event = Key.Repeat)
  | _ -> Alcotest.fail "legacy extension sequence did not produce two keys"

let test_kitty_keyboard () =
  let input = "\027[97:65:113;2:3;65u\027[57399;1u\027[97;5u" in
  match decode input with
  | [ Event.Key shifted; Event.Key keypad; Event.Key ctrl ] ->
      Alcotest.(check string) "shifted code" "shift+a" (Key.to_string shifted);
      Alcotest.(check bool) "shifted alternate" true (Option.is_some shifted.shifted);
      Alcotest.(check bool) "shifted base" true (Option.is_some shifted.base);
      Alcotest.(check bool) "release" true (shifted.event = Key.Release);
      Alcotest.(check string) "keypad" "kp_0" (Key.to_string keypad);
      Alcotest.(check string) "ctrl key has no text" "ctrl+a" (Key.to_string ctrl);
      Alcotest.(check bool) "ctrl text is empty" true (ctrl.text = "")
  | _ -> Alcotest.fail "Kitty keyboard sequence did not produce three keys"

let test_alt_and_timeout () =
  let decoder = Input.create () in
  Alcotest.(check bool) "empty has no pending escape" false (Input.pending_escape decoder);
  Alcotest.(check (list string))
    "alt key" [ "alt+x" ]
    (List.map
       (function Event.Key key -> Key.to_string key | _ -> "other")
       (Input.feed decoder "\027x"));
  Alcotest.(check (list string))
    "bare escape waits" []
    (List.map
       (function Event.Key key -> Key.to_string key | _ -> "other")
       (Input.feed decoder "\027"));
  Alcotest.(check bool) "bare escape pending" true (Input.pending_escape decoder);
  Alcotest.(check (list string))
    "timeout resolves escape" [ "escape" ]
    (List.map
       (function Event.Key key -> Key.to_string key | _ -> "other")
       (Input.flush decoder));
  Alcotest.(check bool) "flush clears pending" false (Input.pending_escape decoder)

let test_incremental_sequences () =
  let decoder = Input.create () in
  Alcotest.(check int) "CSI prefix waits" 0 (List.length (Input.feed decoder "\027["));
  Alcotest.(check int) "CSI completes" 1 (List.length (Input.feed decoder "A"));
  Alcotest.(check int) "UTF8 prefix waits" 0 (List.length (Input.feed decoder "\226"));
  let euro = Input.feed decoder "\130\172" in
  Alcotest.(check string) "UTF8 scalar" "€" (Key.to_string (only_key euro));
  Alcotest.(check int)
    "OSC prefix waits" 0
    (List.length (Input.feed decoder "\027]11;rgb:ff"));
  match Input.feed decoder "00/0000/ffff\007" with
  | [ Event.Background_color (Charm_ansi.Color.Rgb (r, g, b)) ] ->
      Alcotest.(check (triple int int int)) "OSC color" (255, 0, 255) (r, g, b)
  | events -> Alcotest.failf "unexpected OSC events: %d" (List.length events)

let test_mouse_focus_and_reports () =
  let input =
    "\027[<4;10;20m\027[<64;2;3M\027[I\027[O\027[12;34R\027[?7u\027[?1049;1$y"
  in
  match decode input with
  | [
   Event.Mouse release;
   Event.Mouse wheel;
   Event.Focus;
   Event.Blur;
   Event.Cursor_position position;
   Event.Kitty_flags flags;
   Event.Mode_report report;
  ] ->
      Alcotest.(check int) "release x" 9 release.x;
      Alcotest.(check int) "release y" 19 release.y;
      Alcotest.(check bool)
        "release action" true
        (release.action = Charm_tea.Mouse.Release);
      Alcotest.(check bool) "wheel action" true (wheel.action = Charm_tea.Mouse.Press);
      Alcotest.(check int) "wheel y" 2 wheel.y;
      Alcotest.(check (pair int int)) "CPR" (11, 33) (position.row, position.col);
      Alcotest.(check int) "Kitty flags" 7 flags;
      Alcotest.(check int) "DECRPM mode" 1049 report.mode;
      Alcotest.(check int) "DECRPM value" 1 report.value
  | events -> Alcotest.failf "unexpected report event count: %d" (List.length events)

let test_xtversion_and_unknown () =
  let input = "\027P>|kitty 1.2\027\\\027[z\027P?1+zpayload\027\\" in
  match decode input with
  | [ Event.Terminal_version version; Event.Unknown csi; Event.Unknown dcs ] ->
      Alcotest.(check string) "XTVERSION" "kitty 1.2" version;
      Alcotest.(check string) "unknown CSI raw" "\027[z" csi;
      Alcotest.(check string) "unknown DCS raw" "\027P?1+zpayload\027\\" dcs
  | events -> Alcotest.failf "unexpected XTVERSION event count: %d" (List.length events)

let test_paste_and_post_paste_bytes () =
  let decoder = Input.create () in
  let first = Input.feed decoder "\027[200~hello" in
  Alcotest.(check (list string))
    "paste start has no ordinary event" []
    (List.map (function Event.Paste text -> text | _ -> "other") first);
  let events = Input.feed decoder " world\027[201~x" in
  match events with
  | [ Event.Paste text; Event.Key key ] ->
      Alcotest.(check string) "paste payload" "hello world" text;
      Alcotest.(check string) "post-paste key" "x" (Key.to_string key)
  | _ ->
      Alcotest.failf "expected paste plus trailing key, got %d events"
        (List.length events)

let test_caps_preserve_and_segment () =
  let decoder = Input.create () in
  let payload = String.make (65_536 + 17) 'p' in
  let events = Input.feed decoder ("\027[200~" ^ payload ^ "\027[201~") in
  let pieces =
    List.filter_map (function Event.Paste text -> Some text | _ -> None) events
  in
  Alcotest.(check bool) "paste segments exist" true (List.length pieces >= 2);
  Alcotest.(check bool)
    "segments stay bounded" true
    (List.for_all (fun text -> String.length text <= 65_536) pieces);
  Alcotest.(check string) "segments preserve bytes" payload (String.concat "" pieces)

let test_unknown_raw_and_stray_paste_end () =
  match decode "\027[201~\027Oz" with
  | [ Event.Unknown paste_end; Event.Unknown ss3 ] ->
      Alcotest.(check string) "stray paste end" "\027[201~" paste_end;
      Alcotest.(check string) "unknown SS3" "\027Oz" ss3
  | events -> Alcotest.failf "unexpected unknown event count: %d" (List.length events)

let feed_one_byte input =
  let decoder = Input.create () in
  let output = ref [] in
  for index = 0 to String.length input - 1 do
    output := List.rev_append (Input.feed decoder (String.sub input index 1)) !output
  done;
  List.rev !output @ Input.flush decoder

let test_all_protocol_chunk_boundaries () =
  let one_key sequence expected =
    match feed_one_byte sequence with
    | [ Event.Key key ] -> Alcotest.(check string) sequence expected (Key.to_string key)
    | events ->
        Alcotest.failf "sequence %S produced %d events" sequence (List.length events)
  in
  one_key "\027OP" "f1";
  one_key "\027[97;1u" "a";
  one_key "\027[1;5A" "ctrl+up";
  one_key "\027[1$" "shift+home";
  (match feed_one_byte "\155A" with
  | [ Event.Key key ] -> Alcotest.(check string) "8-bit CSI" "up" (Key.to_string key)
  | events -> Alcotest.failf "8-bit CSI produced %d events" (List.length events));
  (match feed_one_byte "\027[M\032+," with
  | [ Event.Mouse mouse ] ->
      Alcotest.(check int) "X10 x" 10 mouse.x;
      Alcotest.(check int) "X10 y" 11 mouse.y
  | events -> Alcotest.failf "X10 produced %d events" (List.length events));
  (match feed_one_byte "\027[<32;4;5M" with
  | [ Event.Mouse mouse ] ->
      Alcotest.(check bool) "motion" true (mouse.action = Charm_tea.Mouse.Motion);
      Alcotest.(check int) "motion x" 3 mouse.x;
      Alcotest.(check int) "motion y" 4 mouse.y
  | events -> Alcotest.failf "motion produced %d events" (List.length events));
  (match feed_one_byte "\027[I\027[O\027[12;34R" with
  | [ Event.Focus; Event.Blur; Event.Cursor_position position ] ->
      Alcotest.(check (pair int int)) "CPR" (11, 33) (position.row, position.col)
  | events -> Alcotest.failf "focus and CPR produced %d events" (List.length events));
  (match feed_one_byte "\144>|version\156" with
  | [ Event.Terminal_version version ] ->
      Alcotest.(check string) "8-bit DCS" "version" version
  | events -> Alcotest.failf "8-bit DCS produced %d events" (List.length events));
  match feed_one_byte "\15711;#010203\007" with
  | [ Event.Background_color (Charm_ansi.Color.Rgb (1, 2, 3)) ] -> ()
  | events -> Alcotest.failf "8-bit OSC produced %d events" (List.length events)

let test_eof_abort_and_unknown_boundaries () =
  let decoder = Input.create () in
  Alcotest.(check (list string))
    "incomplete CSI waits" []
    (List.map
       (function Event.Unknown raw -> raw | _ -> "event")
       (Input.feed decoder "\027["));
  Alcotest.(check (list string))
    "incomplete CSI flushes raw" [ "\027[" ]
    (List.map (function Event.Unknown raw -> raw | _ -> "event") (Input.flush decoder));
  (match feed_one_byte "\027[?1;2c" with
  | [ Event.Unknown raw ] -> Alcotest.(check string) "DA1 raw" "\027[?1;2c" raw
  | events -> Alcotest.failf "DA1 produced %d events" (List.length events));
  (match feed_one_byte "\027]11;#010203\024" with
  | [ Event.Unknown raw ] ->
      Alcotest.(check string) "cancelled OSC raw" "\027]11;#010203\024" raw
  | events -> Alcotest.failf "cancelled OSC produced %d events" (List.length events));
  (match feed_one_byte "\027P>|version\026" with
  | [ Event.Unknown raw ] ->
      Alcotest.(check string) "cancelled DCS raw" "\027P>|version\026" raw
  | events -> Alcotest.failf "cancelled DCS produced %d events" (List.length events));
  let decoder = Input.create () in
  let oversized = "\027[" ^ String.make 65_537 '1' in
  let events = Input.feed decoder oversized in
  let unknown =
    List.filter_map (function Event.Unknown raw -> Some raw | _ -> None) events
  in
  Alcotest.(check string)
    "pending cap preserves bytes" oversized (String.concat "" unknown);
  Alcotest.(check bool)
    "unknown segments bounded" true
    (List.for_all (fun raw -> String.length raw <= 65_536) unknown)

let test_consecutive_pastes_and_marker_chunks () =
  let input = "\027[200~a\027[201~\027[200~b\027[201~" in
  match feed_one_byte input with
  | [ Event.Paste first; Event.Paste second ] ->
      Alcotest.(check string) "first paste" "a" first;
      Alcotest.(check string) "second paste" "b" second
  | events -> Alcotest.failf "consecutive pastes produced %d events" (List.length events)

let cases =
  [
    Alcotest.test_case "plain_and_controls" `Quick test_plain_and_controls;
    Alcotest.test_case "legacy_csi_ss3_urxvt" `Quick test_legacy_csi_ss3_and_urxvt;
    Alcotest.test_case "legacy_modifiers" `Quick
      test_legacy_modifiers_and_kitty_extensions;
    Alcotest.test_case "kitty_keyboard" `Quick test_kitty_keyboard;
    Alcotest.test_case "alt_and_timeout" `Quick test_alt_and_timeout;
    Alcotest.test_case "incremental_sequences" `Quick test_incremental_sequences;
    Alcotest.test_case "mouse_focus_reports" `Quick test_mouse_focus_and_reports;
    Alcotest.test_case "xtversion_unknown" `Quick test_xtversion_and_unknown;
    Alcotest.test_case "paste_post_paste" `Quick test_paste_and_post_paste_bytes;
    Alcotest.test_case "paste_cap" `Quick test_caps_preserve_and_segment;
    Alcotest.test_case "unknown_raw" `Quick test_unknown_raw_and_stray_paste_end;
    Alcotest.test_case "all_protocol_chunk_boundaries" `Quick
      test_all_protocol_chunk_boundaries;
    Alcotest.test_case "eof_abort_unknown_boundaries" `Quick
      test_eof_abort_and_unknown_boundaries;
    Alcotest.test_case "consecutive_pastes" `Quick
      test_consecutive_pastes_and_marker_chunks;
  ]
