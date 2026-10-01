let () =
  Test_support.run_lwt "glow"
    [ Test_config.suite; Test_source.suite; Test_ui.suite; Test_cli.suite ]
