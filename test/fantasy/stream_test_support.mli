open Charm_fantasy

(** Shared stream-part assertions for the provider codec suites.

    The helpers compare the public [Stream_part] event surface and do not expose
    provider-specific decoder state. *)

val finish_name :
  [ `Stop | `Tool_calls | `Length | `Content_filter | `Error of string ] -> string
(** [finish_name f] is the diagnostic name of [f]. *)

val part : Stream_part.t Alcotest.testable
(** [part] compares and prints one stream part with [Stream_part.pp]. *)

val parts : Stream_part.t list Alcotest.testable
(** [parts] compares a list of stream parts. *)

val last : 'a list -> 'a option
(** [last xs] is the last element of [xs], or [None] for an empty list. *)

val is_finish : Stream_part.t -> bool
(** [is_finish p] is true exactly for [Finish _]. *)

val single_finish :
  Stream_part.t list ->
  [ `Stop | `Tool_calls | `Length | `Content_filter | `Error of string ]
(** [single_finish ps] extracts the only finish reason in [ps], failing the current
    Alcotest case when there is not exactly one. *)

val drain : Stream_part.t Eio.Stream.t -> Stream_part.t list
(** [drain stream] takes events until the first terminal [Finish]. *)
