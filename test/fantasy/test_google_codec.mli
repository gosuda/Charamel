(** Behavior cases for the Google [streamGenerateContent] codec.

    The suite drives the public provider against a local HTTP fixture server and observes
    both the posted request body and the stream parts the decoder produces. *)

val cases : unit Alcotest.test_case list
(** [cases] are the test cases for the Google codec. *)
