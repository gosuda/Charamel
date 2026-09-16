(** Alcotest cases for real filesystem navigation and selection. *)

val cases : unit Alcotest.test_case list
(** [cases] covers directory IO, filtering, selection policy, errors, and resize behavior.
*)
