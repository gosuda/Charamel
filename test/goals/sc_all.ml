let () =
  Alcotest.run "goals"
    [
      ("SC-01", [ Alcotest.test_case "release build" `Quick Sc_01.run ]);
      ("SC-02", [ Alcotest.test_case "gum choose" `Quick Sc_02.run ]);
      ("SC-03", [ Alcotest.test_case "crush run" `Quick Sc_03.run ]);
      ("SC-04", [ Alcotest.test_case "keygen" `Quick Sc_04.run ]);
      ("SC-05", [ Alcotest.test_case "glow" `Quick Sc_05.run ]);
    ]
