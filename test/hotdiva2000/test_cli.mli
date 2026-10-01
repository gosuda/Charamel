val suites : (string * unit Alcotest_lwt.test_case list) list
(** Command-line checks for [hotdiva2000].

    The cases execute the built binary through pipes, covering defaults, generation
    options, output framing, and command-line failures. *)
