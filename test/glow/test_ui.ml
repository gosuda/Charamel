let command_split () =
  Alcotest.(check (list string))
    "quoted command" [ "less"; "-R" ]
    (Charamel_os.Shell.split_words "less '-R'");
  Alcotest.(check (list string))
    "spaces" [ "sh"; "-c"; "echo hi" ]
    (Charamel_os.Shell.split_words "sh -c \"echo hi\"")

let key code = Charamel_tea.Key.v (Charamel_tea.Key.Char (Uchar.of_char code))
let enter = Charamel_tea.Key.v Charamel_tea.Key.Enter

let scripted_pager () =
  let events =
    [
      `Key (key '/');
      `Key (key 'H');
      `Key (key 'e');
      `Key (key 'a');
      `Key (key 'd');
      `Key (key 'i');
      `Key (key 'n');
      `Key (key 'g');
      `Key enter;
      `Key (key 'n');
      `Resize (8, 40);
      `Key (key 'q');
    ]
  in
  let frame =
    Ui.scripted ~config:Config.default ~content:"# Heading\n\nSome text" ~events
      ~size:(10, 40)
  in
  Alcotest.(check bool) "rendered heading" true (String.contains frame 'H')

let suite =
  ( "ui",
    [
      Alcotest_lwt.test_case_sync "command split" `Quick command_split;
      Alcotest_lwt.test_case_sync "scripted pager" `Quick scripted_pager;
    ] )
