(** Canonical keyboard key representation tests. *)

val cases : unit Alcotest.test_case list
(** [cases] covers named codes, function keys, modifier combinations, lock-state
    normalization, printable plus and space forms, aliases, parse errors, and matching. *)
