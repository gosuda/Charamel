(** Canonical keyboard key representation tests. *)

val cases : unit Alcotest.test_case list
(** [cases] covers named codes, function keys, modifier combinations, lock-state
    normalization, printable plus and space forms, aliases, parse errors, matching, and a
    randomized [to_string]/[of_string] round trip over printable keys. *)
