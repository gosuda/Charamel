(** Parser test cases for the charamel.ansi suite.

    The cases port the upstream parser decode, DCS, CSI and OSC vectors, then pin the
    incremental, cancel, limit, UTF-8 and flush behaviour of {!Parser}. *)

val cases : unit Alcotest.test_case list
(** [cases] holds one test case per upstream vector or boundary behaviour. *)
