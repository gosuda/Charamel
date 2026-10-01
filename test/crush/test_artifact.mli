(** Filesystem-backed artifact bounds and identity tests. *)

val cases : unit Alcotest_lwt.test_case list
(** [cases] covers threshold, head/tail spilling, load, permissions, and concurrent saves.
*)
