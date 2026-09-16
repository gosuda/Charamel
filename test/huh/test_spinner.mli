(** The alcotest suite for the huh spinner. *)

val tests : unit Alcotest.test_case list
(** [tests] covers the spinner's accessible mode: a successful action returns its value, a
    failed action reports its message, and ctrl-c read from a pseudo-terminal cancels the
    running action fiber and is reported as [Error `Interrupted]. *)
