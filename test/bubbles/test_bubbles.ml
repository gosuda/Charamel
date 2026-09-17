let () =
  Alcotest.run "bubbles"
    [
      ("key binding", Test_key_binding.cases);
      ("fuzzy", Test_fuzzy.cases);
      ("duration", Test_duration.cases);
      ("cursor", Test_cursor.cases);
      ("textinput", Test_textinput.cases);
      ("textarea", Test_textarea.cases);
      ("viewport", Test_viewport.cases);
      ("list", Test_list.cases);
      ("table", Test_table.cases);
      ("spinner", Test_spinner.cases);
      ("progress", Test_progress.cases);
      ("paginator", Test_paginator.cases);
      ("help", Test_help.cases);
      ("timer", Test_timer.cases);
      ("stopwatch", Test_stopwatch.cases);
      ("filepicker", Test_filepicker.cases);
      ("tree", Test_tree.cases);
    ]
