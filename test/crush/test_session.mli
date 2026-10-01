(** Filesystem-backed session replay and corruption tests. *)

val cases : unit Alcotest_lwt.test_case list
(** [cases] covers JSONL event round-trips, index replay, and final-line repair. *)
