let () =
  Test_support.run_lwt "melt"
    [ ("mnemonic", Test_mnemonic.cases); ("cli", Test_melt_cli.cases) ]
