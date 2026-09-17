(** Alcotest driver for the [charamel.tea] suites.

    The driver contributes no cases of its own. It groups the case lists exported by the
    sibling modules into the suites {!suites} names, so the whole Tea runtime is exercised
    by one executable. *)

val suites : (string * unit Alcotest.test_case list) list
(** [suites] is every Tea case module paired with the suite name it is reported under:
    [key] for the canonical key representation, [input] for the incremental decoder,
    [cmd-sub] for the command and subscription builders, [screen] for the grid diff,
    [terminal] for the transport boundary and [program] for the production run loop. *)
