let suites =
  [
    ("key", Test_key.cases);
    ("input", Test_input.cases);
    ("cmd-sub", Test_cmd_sub.cases);
    ("screen", Test_screen.cases);
    ("terminal", Test_terminal.cases);
    ("program", Test_program.cases);
  ]

let () = Alcotest.run "tea" suites
