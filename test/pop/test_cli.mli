(** CLI-facing composition cases for [pop]. *)

val cases : unit Alcotest_lwt.test_case list
(** [cases] covers recipient parsing and delivery-free preview preparation. *)
