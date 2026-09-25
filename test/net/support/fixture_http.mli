(** Loopback HTTP fixture with scripted responses and real chunk boundaries.

    One fixture per case: {!val:with_server} binds [127.0.0.1:0], serves the queued
    scripts over fresh connections, and shuts the listener down when the case body
    finishes. The wire is written by hand, so a case controls exactly what the client
    sees: chunked framing flushed piece by piece, a pause that leaves a response
    half-finished, a [Retry-After] header, a delayed status line, and a body cut short
    mid-transfer. *)

type t
(** The type for a running fixture server, built only by {!val:with_server}. *)

val with_server : (t -> unit Lwt.t) -> unit Lwt.t
(** [with_server f] starts a fixture, runs [f] against it, then stops the listener and the
    live connection fibers. *)

val uri : t -> string -> Uri.t
(** [uri t path] is the request URI for [path] against the bound port. *)

val respond :
  t ->
  ?status:int ->
  ?headers:(string * string) list ->
  ?retry_after:float ->
  ?before:float ->
  ?truncate:int ->
  string ->
  unit
(** [respond t ?status ?headers ?retry_after ?before ?truncate body] queues one
    [content-length] response. [status] defaults to 200 and [content-type] to
    [text/event-stream] unless [headers] sets it; [headers] are sent verbatim.
    [retry_after] adds a [Retry-After] header counting seconds, [before] delays the status
    line, and [truncate] sends only the first [truncate] body bytes before closing. *)

val respond_chunks :
  t ->
  ?status:int ->
  ?headers:(string * string) list ->
  ?before:float ->
  ?hold:float ->
  string list ->
  unit
(** [respond_chunks t ?status ?headers ?before ?hold chunks] queues one
    [transfer-encoding: chunked] response carrying each element as its own wire chunk,
    flushed separately. [before] delays the status line and [hold] pauses before the
    terminating chunk, leaving the response half-finished for timeout and incremental
    delivery cases. *)

val requests : t -> int
(** [requests t] is the number of requests served so far. *)

val last_path : t -> string option
(** [last_path t] is the request path of the most recent request, if any. *)

val last_body : t -> string option
(** [last_body t] is the request body of the most recent request, if any. *)

val last_headers : t -> (string * string) list
(** [last_headers t] are the lowercased header fields of the most recent request. *)
