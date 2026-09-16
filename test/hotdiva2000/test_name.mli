(** Alcotest suites for {!Name}: exact-output tests over a controlled entropy queue (a
    fake [random] source, never the real RNG), plus boundary validation. Word-list
    transcription fidelity ({!Words}) is verified once elsewhere and is not duplicated
    here.

    Exposed rather than run directly. {!suites} is meant to be spliced into a single
    [Alcotest.run] call owned by whatever [test/hotdiva2000/dune] wires up as the runnable
    test executable for this app, alongside any sibling suites (e.g. a future CLI-level
    test module). *)

val suites : (string * unit Alcotest.test_case list) list
