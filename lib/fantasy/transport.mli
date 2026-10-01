(** The transport for provider calls.

    Every request runs over {!Charamel_net.call_raw}, which owns the connection, the TLS
    trust decision, the size bounds, and the [Retry-After] resolution. Retries follow
    {!Retry.default}. *)

type sse_feed = event:string -> data:string -> unit
(** The type for an SSE event callback: one complete [event] with its joined [data]. *)

val call :
  url:string ->
  headers:(string * string) list ->
  body:string ->
  on_event:sse_feed ->
  (unit, Error.t) result Lwt.t
(** [call ~url ~headers ~body ~on_event] POSTs [body] to [url] and parses the response as
    an SSE stream, calling [on_event] per complete event inside the call.

    [headers] gain [content-type: application/json] when they carry no content type. The
    result is [Ok ()] when the status is 2xx and the stream ended; a non-2xx status
    returns [`Http] with [retryable] from [Retry.retryable_status] and the
    [x-should-retry] header honoured; connection failures return [`Transport]. A request
    is retried under {!Retry.default} only while no event was delivered: a stream whose
    events already reached [on_event] is never re-issued. *)
