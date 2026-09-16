(** A loopback HTTP fixture server for provider tests.

    [t] serves scripted responses, records the last request's path, body, and headers, and
    supports truncation for premature-EOF tests. One instance per test module; workers
    share it. *)

type t
(** The type for a running fixture server. *)

val start : sw:Eio.Switch.t -> net:_ Eio.Net.t -> unit -> t
(** [start ~sw ~net ()] binds [127.0.0.1:0] and serves scripted responses in a fiber until
    [sw] finishes. *)

val base_url : t -> string
(** [base_url t] is the server origin, [http://127.0.0.1:<port>]. *)

val respond : t -> ?status:int -> ?retry_after:float -> string -> unit
(** [respond t ?status ?retry_after body] queues [body] as the next response. [status]
    defaults to [200]. The response content type is [text/event-stream]. When supplied,
    [retry_after] adds a [Retry-After] header carrying seconds. *)

val respond_with :
  t ->
  status:int ->
  ?retry_after:float ->
  headers:(string * string) list ->
  string ->
  unit
(** [respond_with t ~status ?retry_after ~headers body] queues a response with an
    arbitrary status and extra headers. *)

val queue : t -> (int * string) list -> unit
(** [queue t responses] replaces the pending responses with a FIFO of [(status, body)]
    pairs served in order. *)

val trunc : t -> int -> unit
(** [trunc t n] makes the next response send only its first [n] bytes and then close the
    connection early. *)

val last_path : t -> string option
(** [last_path t] is the path and query of the most recent request, [None] before any
    request arrived. *)

val last_body : t -> string option
(** [last_body t] is the raw request body of the most recent request. *)

val last_headers : t -> (string * string) list
(** [last_headers t] are the request headers with lowercased names. *)

val request_count : t -> int
(** [request_count t] is how many requests have arrived. *)
