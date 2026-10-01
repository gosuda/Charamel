(** End-to-end MCP transport contract tests. *)

val cases : unit Alcotest_lwt.test_case list
(** [cases] exercises real stdio and streamable HTTP/SSE fixtures, including a
    server-initiated request answered over the HTTP reply path. *)
