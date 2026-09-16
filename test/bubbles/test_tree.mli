(** Alcotest cases for the expandable tree browser. *)

val cases : unit Alcotest.test_case list
(** [cases] covers node size/open state, navigation, keymaps, help, and viewport
    rendering. *)
