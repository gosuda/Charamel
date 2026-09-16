(** The pluggable transport boundary for provider calls.

    [t] bundles an HTTP client, a retry policy, and a clock. Provider constructors build
    one from an [Eio.Net.t]; tests build one against a fixture server. The transport owns
    header composition, the retry loop with [Retry-After] handling, and bounded response
    reading. *)

type t
(** The type for a transport. *)

type sse_feed = event:string -> data:string -> unit
(** The type for an SSE event callback: one complete [event] with its joined [data]. *)

(* How a response body is consumed. [Sse on_event] parses the body as
   an SSE stream and calls [on_event] per complete event; [String] is
   collected whole and returned. *)
type consume = Sse of sse_feed | String

type response = { status : int; headers : (string * string) list; body : string }
(** The type for a fully-read response: [status] code, response [headers] with lowercased
    names, and the [body] for [String] consumption (empty for [Sse]). *)

val make :
  ?retry:Retry.t ->
  ?headers:(string * string) list ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  unit ->
  t
(** [make ?retry ?headers ~clock ~net ()] builds a transport.

    [retry] defaults to [Retry.default], [headers] to [[]]; they apply to every request as
    base headers. *)

val call :
  t ->
  sw:Eio.Switch.t ->
  url:string ->
  meth:[ `GET | `POST ] ->
  headers:(string * string) list ->
  ?body:string ->
  consume:consume ->
  unit ->
  (response, Error.t) result
(** [call t ~sw ~url ~meth ~headers ?body ~consume ()] performs one HTTP request with the
    transport's retry policy.

    [body] defaults to [""] and is sent only for POSTs. The result is [Ok] when the status
    is 2xx; a non-2xx status returns [`Http] with [retryable] from
    [Retry.retryable_status] and the [x-should-retry] header honoured; connection failures
    return [`Transport]. SSE events are delivered through [consume] inside the call. *)

val post :
  t ->
  sw:Eio.Switch.t ->
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, Error.t) result
(** [post t ~sw ~url ~headers ~body] is a simple JSON POST returning the status and
    response body as a pair. *)
