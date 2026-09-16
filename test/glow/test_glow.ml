let () =
  Alcotest.run "glow"
    [ Test_config.suite; Test_source.suite; Test_ui.suite; Test_cli.suite ]
