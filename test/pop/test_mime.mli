(** Alcotest cases for RFC 5322/MIME message serialisation. *)

val cases : unit Alcotest.test_case list
(** [cases] holds deterministic plain, alternative, attachment, encoded-subject, envelope
    and header-injection checks. *)
