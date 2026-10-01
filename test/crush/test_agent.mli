(** Agent integration tests against a loopback Anthropic HTTP stream fixture. *)

val cases : unit Alcotest_lwt.test_case list
(** [cases] is the list of agent loop contract tests. *)
