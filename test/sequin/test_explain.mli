(** Explain tests. *)

val cases : unit Alcotest_lwt.test_case list
(** [cases] are the [Explain] test cases, assembled into the sequin suite by the test
    driver. *)
