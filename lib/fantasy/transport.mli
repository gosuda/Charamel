(** The transport for provider calls.

    {!t} holds a retry policy and base headers; every request runs over
    {!Charamel_net.call}, which owns the connection, the TLS trust decision, the size
    bounds, and the [Retry-After] resolution. *)

type t
(** The type for a transport. *)

type sse_feed = event:string -> data:string -> unit
(** The type for an SSE event callback: one complete [event] with its joined [data]. *)

(* How a response body is consumed. [Sse on_event] parses the body as an SSE stream and
   calls [on_event] per complete event; [String] is collected whole and returned. *)
type consume = Sse of sse_feed | String

type response = { status : int; headers : (string * string) list; body : string }
(** The type for a fully-read response: [status] code, response [headers] with lowercased
    names, and the [body] for [String] consumption (empty for [Sse]). *)

val make : ?retry:Retry.t -> ?headers:(string * string) list -> unit -> t
(** [make ?retry ?headers ()] builds a transport.

    [retry] defaults to [Retry.default], [headers] to [[]]; they apply to every request as
    base headers. *)

val call :
  t ->
  url:string ->
  meth:[ `GET | `POST ] ->
  headers:(string * string) list ->
  ?body:string ->
  consume:consume ->
  unit ->
  (response, Error.t) result Lwt.t
(** [call t ~url ~meth ~headers ?body ~consume ()] performs one HTTP request with the
    transport's retry policy.

    [body] defaults to [""] and is sent only for POSTs. The result is [Ok] when the status
    is 2xx; a non-2xx status returns [`Http] with [retryable] from
    [Retry.retryable_status] and the [x-should-retry] header honoured; connection failures
    return [`Transport]. SSE events are delivered through [consume] inside the call. A
    stream whose events were already partly delivered is never re-issued. *)

val post :
  t ->
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, Error.t) result Lwt.t
(** [post t ~url ~headers ~body] is a simple JSON POST returning the status and response
    body as a pair. *)
