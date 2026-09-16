let () =
  Alcotest.run "melt" [ ("mnemonic", Test_mnemonic.cases); ("cli", Test_melt_cli.cases) ]
