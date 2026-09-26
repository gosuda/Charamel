(** XDG directory tests. *)

val cases : unit Alcotest_lwt.test_case list
(** [cases] are the [Charamel_cli.Xdg] test cases, assembled into the cli suite by the
    test driver. *)
