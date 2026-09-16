(** Filesystem-backed artifact bounds and identity tests. *)

val cases : unit Alcotest.test_case list
(** [cases] covers threshold, head/tail spilling, load, permissions, and concurrent saves.
*)
