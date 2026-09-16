module Key = Charm_tea.Key

let no_mods =
  {
    Key.shift = false;
    alt = false;
    ctrl = false;
    meta = false;
    super = false;
    hyper = false;
    caps_lock = false;
    num_lock = false;
  }

let mods_of_bits bits =
  {
    Key.shift = bits land 1 <> 0;
    alt = bits land 2 <> 0;
    ctrl = bits land 4 <> 0;
    meta = bits land 8 <> 0;
    super = bits land 16 <> 0;
    hyper = bits land 32 <> 0;
    caps_lock = bits land 64 <> 0;
    num_lock = bits land 128 <> 0;
  }

let named_codes =
  [
    Key.Enter;
    Key.Tab;
    Key.Backspace;
    Key.Escape;
    Key.Space;
    Key.Insert;
    Key.Delete;
    Key.Up;
    Key.Down;
    Key.Left;
    Key.Right;
    Key.Home;
    Key.End;
    Key.Page_up;
    Key.Page_down;
    Key.Kp_0;
    Key.Kp_1;
    Key.Kp_2;
    Key.Kp_3;
    Key.Kp_4;
    Key.Kp_5;
    Key.Kp_6;
    Key.Kp_7;
    Key.Kp_8;
    Key.Kp_9;
    Key.Kp_decimal;
    Key.Kp_divide;
    Key.Kp_multiply;
    Key.Kp_subtract;
    Key.Kp_add;
    Key.Kp_enter;
    Key.Kp_equal;
    Key.Kp_begin;
    Key.Caps_lock;
    Key.Scroll_lock;
    Key.Num_lock;
    Key.Print_screen;
    Key.Pause;
    Key.Menu;
    Key.Media_play;
    Key.Media_pause;
    Key.Media_play_pause;
    Key.Media_stop;
    Key.Media_next;
    Key.Media_prev;
    Key.Media_record;
    Key.Media_fast_forward;
    Key.Media_rewind;
    Key.Volume_up;
    Key.Volume_down;
    Key.Volume_mute;
    Key.Left_shift;
    Key.Left_ctrl;
    Key.Left_alt;
    Key.Left_super;
    Key.Left_hyper;
    Key.Left_meta;
    Key.Right_shift;
    Key.Right_ctrl;
    Key.Right_alt;
    Key.Right_super;
    Key.Right_hyper;
    Key.Right_meta;
    Key.Iso_level3_shift;
    Key.Iso_level5_shift;
  ]

let function_codes = List.init 63 (fun n -> Key.F (n + 1))

let check_roundtrip code mods =
  let expected = Key.v ~mods code in
  let printed = Key.to_string expected in
  match Key.of_string printed with
  | Error (`Msg message) -> Alcotest.failf "Key.of_string %S failed: %s" printed message
  | Ok actual ->
      Alcotest.(check bool)
        printed true
        (actual.code = expected.code && actual.mods = expected.mods && actual.text = ""
       && actual.shifted = None && actual.base = None && actual.event = Key.Press)

let test_named_roundtrips () =
  let all_codes = named_codes @ function_codes in
  List.iter
    (fun code ->
      for bits = 0 to 255 do
        check_roundtrip code (mods_of_bits bits)
      done)
    all_codes

let test_printable_roundtrips () =
  let chars =
    [
      Uchar.of_char 'a';
      Uchar.of_char 'Z';
      Uchar.of_char '+';
      Uchar.of_char ' ';
      Uchar.of_int 0x1f600;
    ]
  in
  List.iter
    (fun scalar ->
      for bits = 0 to 255 do
        check_roundtrip (Key.Char scalar) (mods_of_bits bits)
      done)
    chars

let test_lock_and_modifier_normalization () =
  let all = mods_of_bits 255 in
  let cases : (Key.code * (Key.mods -> bool)) list =
    [
      (Key.Left_ctrl, fun mods -> not mods.ctrl);
      (Key.Right_alt, fun mods -> not mods.alt);
      (Key.Left_shift, fun mods -> not mods.shift);
      (Key.Caps_lock, fun mods -> not mods.caps_lock);
      (Key.Num_lock, fun mods -> not mods.num_lock);
    ]
  in
  List.iter
    (fun (code, predicate) ->
      let key = Key.v ~mods:all code in
      Alcotest.(check bool) (Key.to_string key) true (predicate key.mods))
    cases

let test_plus_and_aliases () =
  let cases =
    [
      ("+", "+");
      ("plus", "+");
      ("ctrl+plus", "ctrl+plus");
      ("alt+shift+plus", "alt+shift+plus");
      ("caps_lock+plus", "caps_lock+plus");
    ]
  in
  List.iter
    (fun (printed, canonical) ->
      match Key.of_string printed with
      | Error (`Msg message) -> Alcotest.failf "alias %S failed: %s" printed message
      | Ok key -> Alcotest.(check string) printed canonical (Key.to_string key))
    cases;
  Alcotest.(check string)
    "modified plus uses an unambiguous name" "ctrl+plus"
    (Key.to_string
       (Key.v ~mods:{ no_mods with ctrl = true } (Key.Char (Uchar.of_char '+'))))

let test_aliases_and_errors () =
  let aliases =
    [ ("esc", Key.Escape); ("pgup", Key.Page_up); ("pgdown", Key.Page_down) ]
  in
  List.iter
    (fun (name, code) ->
      match Key.of_string name with
      | Ok key -> Alcotest.(check bool) name true (key.code = code && key.mods = no_mods)
      | Error (`Msg message) -> Alcotest.failf "alias %S failed: %s" name message)
    aliases;
  List.iter
    (fun input ->
      match Key.of_string input with
      | Error (`Msg _) -> ()
      | Ok _ -> Alcotest.failf "invalid binding %S was accepted" input)
    [ ""; "ctrl"; "a+b"; "unknown"; "shift+" ]

let test_matches_ignores_payload () =
  let first = Key.v ~mods:{ no_mods with ctrl = true } (Key.Char (Uchar.of_char 'c')) in
  let second =
    { first with Key.text = "C"; shifted = Some (Uchar.of_char 'C'); event = Key.Release }
  in
  Alcotest.(check bool)
    "payload fields do not affect matching" true (Key.matches first second)

let cases =
  [
    Alcotest.test_case "named_roundtrips" `Quick test_named_roundtrips;
    Alcotest.test_case "printable_roundtrips" `Quick test_printable_roundtrips;
    Alcotest.test_case "lock_normalization" `Quick test_lock_and_modifier_normalization;
    Alcotest.test_case "plus_aliases" `Quick test_plus_and_aliases;
    Alcotest.test_case "aliases_and_errors" `Quick test_aliases_and_errors;
    Alcotest.test_case "matches_payload" `Quick test_matches_ignores_payload;
  ]
