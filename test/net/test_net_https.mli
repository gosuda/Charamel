val cases : unit Alcotest_lwt.test_case list
(** The HTTPS trust-store cases: a loopback TLS server proves [https] loads the anchors
    while rejecting a chain outside the store. *)
