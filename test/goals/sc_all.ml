let () =
  Alcotest.run "goals"
    [
      ( "SC-01",
        [
          Alcotest.test_case "release build" `Quick (fun () ->
              Alcotest.fail
                "SC-01 unmet: dune build --profile release exits 0 with every package");
        ] );
      ( "SC-02",
        [
          Alcotest.test_case "gum choose" `Quick (fun () ->
              Alcotest.fail
                "SC-02 unmet: printf 'b\\n' | gum choose --select-if-one prints b and \
                 exits 0");
        ] );
      ( "SC-03",
        [
          Alcotest.test_case "crush run" `Quick (fun () ->
              Alcotest.fail
                "SC-03 unmet: crush run say hi against a mock provider prints the reply");
        ] );
      ( "SC-04",
        [
          Alcotest.test_case "keygen" `Quick (fun () ->
              Alcotest.fail
                "SC-04 unmet: keygen -t ed25519 -f /tmp/k writes an OpenSSH key pair \
                 whose fingerprint ssh-keygen -l confirms");
        ] );
      ( "SC-05",
        [
          Alcotest.test_case "glow" `Quick (fun () ->
              Alcotest.fail
                "SC-05 unmet: glow README.md prints the H1 styled by the dark theme");
        ] );
    ]
