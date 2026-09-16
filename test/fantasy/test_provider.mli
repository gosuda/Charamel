(** Public-provider transport tests against the loopback HTTP fixture.

    [cases] exercises SSE framing, request headers, retry policy, malformed responses, and
    cancellation without importing private codecs. *)

val cases : unit Alcotest.test_case list
(** [cases] is the transport test suite. *)
