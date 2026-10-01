(** HTTP client with streaming responses, and the SSH server transport.

    [Charamel_net] is the one module that hides the transport decision: [cohttp-lwt]
    request and response framing over conduit's pure-OCaml TLS ([tls-lwt] with [ca-certs]
    trust anchors — never OpenSSL, never [lwt_ssl]), plus {!module:Ssh_server} for SSH
    servers on awa-mirage.

    Recoverable failures travel as [(value, error)] results; only programming errors
    raise. Every result carries at least the {!type:error} row, so a consumer that adds
    its own failure kinds keeps one error type end to end.

    Size bounds are enforced at the protocol boundary, never trusted from the peer: one
    SSE line above {!val:max_sse_line}, one event's data above {!val:max_sse_event}, and
    one response body above {!val:max_http_body} each fail the read instead of growing
    without limit.

    Timeout semantics. [timeout] bounds a single attempt — connect, request, response head
    — through [Lwt_unix.with_timeout] and defaults to {!val:attempt_timeout}; it must be
    positive and finite, otherwise [Invalid_argument]. A timed-out attempt closes its
    connection and reports [Transport "request timed out"]. Body reads are bounded
    separately: {!val:read_body} applies [timeout] to the whole body, while
    {!val:read_sse} and {!val:read_line} apply it per line, so a streaming response is
    only as idle-tolerant as its per-line timeout.

    Retry semantics. {!val:retry_after} resolves a response's [Retry-After] (and
    [retry-after-ms]) header to a delay; {!val:retry} drives the loop, retrying only
    {!val:retryable_error} results and honouring that delay through {!module:Retry}. The
    retried action must be safe to re-issue: retries happen before any response body is
    consumed, so a caller that streams must retry inside its own action. *)

type http_error = {
  status : int;
  title : string;
  message : string;
  retryable : bool;
  retry_after : float option;
}
(** The type for a failed HTTP response: [status] code, [title] the canonical reason
    phrase, [message] a sanitized excerpt of the response body (at most
    {!val:max_error_body} bytes, control bytes replaced by spaces, suffixed with [...]
    when truncated, or ["<response body omitted: too large>"] when the body exceeded
    {!val:max_error_body}), [retryable] per {!val:Retry.retryable_status} or an
    [x-should-retry] header, and [retry_after] the delay the response asked for. *)

type error =
  [ `Oauth of string
  | `Oauth_invalid_grant of string
  | `Http of http_error
  | `Transport of string ]
(** The type for transport failures.

    [`Http] carries a well-formed response with a non-success status. [`Transport] carries
    a connection-level, TLS, timeout, or size-bound failure. [`Oauth] covers OAuth
    failures that do not identify an invalid refresh grant and [`Oauth_invalid_grant] a
    rejected or expired grant; both are produced by authentication flows layered on top of
    {!val:call}, so a provider can carry one error type end to end. *)

val pp_error : error Fmt.t
(** [pp_error] formats the failure for diagnostics. *)

val error_message : error -> string
(** [error_message e] is the human-readable failure text without the constructor label. *)

val retryable_error : error -> bool
(** [retryable_error e] is [true] for [Transport] failures (nothing reached the peer's
    application layer) and for [Http] errors whose [retryable] field is [true]; [false]
    for the OAuth kinds. *)

val max_sse_line : int
(** [max_sse_line] is 64 KiB: the largest single SSE line {!val:read_sse} accepts. *)

val sse_event_name_bound : int
(** [sse_event_name_bound] is 256 bytes: the longest accepted [event:] field. *)

val max_sse_event : int
(** [max_sse_event] is 1 MiB: the largest accumulated [data] of one SSE event. *)

val max_http_body : int
(** [max_http_body] is 10 MiB: the largest response body {!val:read_body} accepts. *)

val max_error_body : int
(** [max_error_body] is 4 KiB: the largest response body read to build an [Http] message.
*)

val attempt_timeout : float
(** [attempt_timeout] is 30 seconds: the default bound for one attempt and one body read.
*)

val retry_after : ?now:float -> Cohttp.Response.t -> float option
(** [retry_after ?now response] is the delay the server asks for, in seconds:
    [retry-after-ms] takes precedence over [Retry-After]; a value is a non-negative finite
    number of seconds or an HTTP-date resolved against [now] (seconds since the Unix
    epoch, defaulting to the wall clock), clamped at [0.]; a missing, malformed, negative,
    or non-finite value gives [None]. No policy ceiling is applied here — see
    {!val:Retry.delay}. *)

val call :
  ?timeout:float ->
  ?headers:(string * string) list ->
  meth:Cohttp.Code.meth ->
  body:string option ->
  Uri.t ->
  (Cohttp.Response.t * string Lwt_stream.t, [> error ]) result Lwt.t
(** [call ?timeout ?headers ~meth ?body uri] issues one request and streams the response.
    [headers] are sent verbatim; [body] is sent only when supplied. The returned stream
    yields response-body chunks as they arrive, so [text/event-stream] responses are
    consumed incrementally by {!val:read_sse}.

    A 2xx response yields [(response, stream)]; any other status yields
    [Error (`Http http_error)] after reading at most {!val:max_error_body} body bytes, and
    the connection is closed. On 2xx the stream owns the connection: draining it to the
    end or a stream failure releases the socket, and a stream dropped unread is released
    by the collector. A caller that knows it will stop reading early should prefer
    {!val:call_raw}, whose channel close releases the connection immediately. [timeout]
    defaults to {!val:attempt_timeout} and bounds the attempt, not the stream.

    @raise Invalid_argument
      when [timeout] is not positive and finite. A malformed URI, a TLS setup failure, and
      every connection-level or timeout failure are [Transport] errors. *)

val call_raw :
  ?timeout:float ->
  ?headers:(string * string) list ->
  meth:Cohttp.Code.meth ->
  body:string option ->
  Uri.t ->
  (Cohttp.Response.t * Lwt_io.input_channel, [> error ]) result Lwt.t
(** [call_raw] is {!val:call} with the body as an [Lwt_io.input_channel] for callers that
    do their own framing — the MCP SSE transport, which reads lines until its answer
    arrives. Closing the channel releases the connection immediately; leaving a channel
    open with bytes unread keeps the connection for the process lifetime. *)

val read_body : ?timeout:float -> string Lwt_stream.t -> (string, [> error ]) result Lwt.t
(** [read_body ?timeout stream] drains [stream] into one string, failing at
    [max_http_body] with [Transport "HTTP response body exceeds 10 MiB"]. [timeout]
    (default {!val:attempt_timeout}) bounds the whole read. *)

type sse_event = { event : string; data : string }
(** The type for one dispatched SSE event: [event] is the [event:] field, defaulting to
    ["message"], and [data] is the [data:] fields joined by newlines. *)

val read_sse :
  ?timeout:float ->
  string Lwt_stream.t ->
  on_event:(sse_event -> unit) ->
  (unit, [> error ]) result Lwt.t
(** [read_sse ?timeout stream ~on_event] parses [stream] as [text/event-stream] and calls
    [on_event] for each event, dispatching a pending event at end of stream. Blank lines
    end an event, [":"] comment lines are ignored, and one trailing carriage return per
    line is stripped.

    [timeout] (default {!val:attempt_timeout}) bounds the wait for one line. Failures are
    [Transport] with: ["timed out waiting for an SSE event"], ["SSE line exceeds 64 KiB"],
    ["SSE event name exceeds 256 bytes"], or ["SSE event data exceeds 1 MiB"]. *)

val read_line :
  ?bound:int ->
  ?timeout:float ->
  Lwt_io.input_channel ->
  (string option, [> error ]) result Lwt.t
(** [read_line ?bound ?timeout ic] reads one line, dropping the line terminator and one
    carriage return before it, and returns [None] at a clean end of input. [bound]
    defaults to {!val:max_sse_line}; a longer line fails with a [Transport] message naming
    the bound. [timeout] bounds the read of one line. *)

val retry :
  ?policy:Retry.t ->
  ?sleep:(float -> unit Lwt.t) ->
  (unit -> ('a, ([> error ] as 'e)) result Lwt.t) ->
  ('a, 'e) result Lwt.t
(** [retry ?policy ?sleep action] runs [action ()] until it succeeds or until a failure is
    not retryable — {!val:Retry.retryable_status} statuses and [Transport] failures are,
    [Oauth] failures and any kind a consumer added are not — at most [policy.max] retries
    after the first attempt. The wait is {!val:Retry.delay} with the failure's
    [retry_after] when it carries one; [sleep] defaults to [Lwt_unix.sleep]. [action] must
    be safe to re-issue. *)

module Retry : module type of Retry
(** Retry policy: exponential backoff with jitter, honouring [Retry-After]. *)

module Ssh_server : module type of Ssh_server
(** SSH server transport: awa-mirage over [Lwt_unix] sockets. *)
