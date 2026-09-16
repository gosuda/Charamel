let () =
  Alcotest.run "charm.fantasy"
    [
      ("anthropic codec", Test_anthropic_codec.cases);
      ("google codec", Test_google_codec.cases);
      ("openai compat codec", Test_openai_compat_codec.cases);
      ("responses codec", Test_responses_codec.cases);
      ("catalog", Test_catalog.cases);
      ("oauth", Test_oauth.cases);
      ("retry", Test_retry.cases);
      ("provider transport", Test_provider.cases);
    ]
