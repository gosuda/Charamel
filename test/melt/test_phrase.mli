(** Alcotest cases for {!Melt_phrase.Phrase}. *)

val cases : unit Alcotest_lwt.test_case list
(** [cases] holds the piped-input, terminal line-join, and echo-failure refusal cases. *)
