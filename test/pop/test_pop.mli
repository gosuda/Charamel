(** Integration tests for [pop] composition and delivery. *)

val suite : (string * unit Alcotest_lwt.test_case list) list
(** [suite] is the set of pop tests. *)
