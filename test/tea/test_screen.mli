(** The alcotest suite for {!Screen}. *)

val cases : unit Alcotest.test_case list
(** [cases] exercises {!Screen.render}, {!Screen.clear}, {!Screen.reset} and
    {!Screen.restore} against exact escape-sequence goldens: a three-frame inline sequence
    (full render, a one-line change, a resize), a wide-grapheme overwrite and its
    determinism, OSC 8 and SGR cell equality across frames, mode-only deltas, the Kitty
    keyboard and alternate-screen transitions, the application cursor, and the
    [reset]/[restore]/[clear] seams. *)
