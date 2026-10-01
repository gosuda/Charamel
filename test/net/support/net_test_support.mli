(** Shared assertions for the [charamel.net] suite.

    Results are compared structurally, so [Http] failures must match field for field and
    [Transport] failures message for message: the wording at the protocol boundary is the
    contract these tests pin. The checks return promises so a case chains them with the
    network calls it surrounds. *)

val events : (string * string) list Alcotest.testable
(** [events] compares dispatched SSE events as [(event name, data)] pairs. *)

val read_events :
  string Lwt_stream.t ->
  ((unit, [> Charamel_net.error ]) result * (string * string) list) Lwt.t
(** [read_events stream] runs {!val:Charamel_net.read_sse} over [stream] with the default
    timeout, returning its result and the events dispatched so far, in order. *)

val collect : 'a Lwt_stream.t -> 'a list Lwt.t
(** [collect stream] drains [stream] into a list of the chunks it yielded, in order. *)

val check_transport :
  string -> string -> (_, [> Charamel_net.error ]) result -> unit Lwt.t
(** [check_transport name message result] passes when [result] is [Error (`Transport m)]
    with [m = message]. *)

val check_http :
  string ->
  int ->
  string ->
  string ->
  bool ->
  float option ->
  (_, [> Charamel_net.error ]) result ->
  unit Lwt.t
(** [check_http name status title message retryable retry_after result] passes when
    [result] is [Error (`Http e)] with every field of [e] equal to the expected values. *)

val check_ok :
  string ->
  'a Alcotest.testable ->
  ('a, [> Charamel_net.error ]) result ->
  'a ->
  unit Lwt.t
(** [check_ok name testable result expected] passes when [result] is [Ok expected]. *)

val value_of : string -> ('a, [> Charamel_net.error ]) result -> 'a
(** [value_of name result] is the success payload, ending the test over any failure. *)

val stream_of : string -> (Cohttp.Response.t * 'a, [> Charamel_net.error ]) result -> 'a
(** [stream_of name result] is the response payload of a successful [result]. *)

val status_code :
  string -> (Cohttp.Response.t * 'a, [> Charamel_net.error ]) result -> int
(** [status_code name result] is the status code of a successful [result]. *)
