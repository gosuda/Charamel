(** Alcotest cases for the SMTP command/reply state machine. *)

val cases : unit Alcotest.test_case list
(** [cases] holds multiline-EHLO, authentication fallback, DATA dot-stuffing, recipient
    and envelope validation, and malformed-reply checks. *)
