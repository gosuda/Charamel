(** Behavior cases for the model catalog.

    The suite checks the embedded snapshot's pricing and provider stamping, the codec
    roundtrip on a fixture body, and the conditional response boundary of [Catalog.fetch]
    against the shared HTTP fixture server. *)

val cases : unit Alcotest.test_case list
(** [cases] are the test cases for the catalog. *)
