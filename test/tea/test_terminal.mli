(** Terminal transport boundary tests.

    The cases cover custom-flow ownership, local pipe fallbacks, descriptor selection, and
    raw-mode restoration on a real pseudoterminal. *)

val cases : unit Alcotest.test_case list
(** [cases] is the set of terminal transport boundary tests. *)
