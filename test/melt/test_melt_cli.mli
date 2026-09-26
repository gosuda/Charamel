(** End-to-end tests for the [melt] backup and restore commands. *)

val cases : unit Alcotest_lwt.test_case list
(** [cases] are the real-filesystem CLI cases added by the melt test runner. *)
