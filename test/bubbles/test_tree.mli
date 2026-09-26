(** Alcotest cases for the expandable tree browser. *)

val cases : unit Alcotest_lwt.test_case list
(** [cases] covers node size/open state, navigation, keymaps, help, and viewport
    rendering. *)
