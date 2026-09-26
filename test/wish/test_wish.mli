(** In-process protocol tests for the Wish SSH server. *)

val suites : unit -> unit Alcotest_lwt.test list
(** [suites ()] is the behavioral Wish test suite. *)
