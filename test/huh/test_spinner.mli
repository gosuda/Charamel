(** The alcotest suite for the huh spinner. *)

val run_pty_child : unit -> 'a
(** [run_pty_child ()] is the second entry point of the test executable: the process the
    pseudo-terminal case starts runs the spinner on its own terminal, sleeps in the
    action, reports the outcome, and exits. *)

val tests : unit Alcotest_lwt.test_case list
(** [tests] covers the spinner's accessible mode: a successful action returns its value, a
    failed action reports its message, and ctrl-c read from a pseudo-terminal cancels the
    running action and is reported as [Error `Interrupted]. *)
