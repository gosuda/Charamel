(** End-to-end MCP transport contract tests. *)

val cases : unit Alcotest.test_case list
(** [cases] exercises real stdio and streamable HTTP/SSE fixtures, including a
    server-initiated request answered over the HTTP reply path. *)
