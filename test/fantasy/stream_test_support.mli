open Charamel_fantasy

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

val drain_queued : Stream_part.t Eio.Stream.t -> Stream_part.t list
(** [drain_queued stream] is every event already queued in [stream], collected without
    waiting. Once the producing switch has closed, this is the complete tail emitted after
    the consumer stopped taking. *)

val parse_body : string option -> Jsont.json
(** [parse_body body] is the JSON value decoded from [body]. *)

val member : string -> Jsont.json -> Jsont.json option
(** [member name json] is the JSON member called [name] in [json], or [None] if [json] has
    no such member. *)

val required : string -> Jsont.json -> Jsont.json
(** [required name json] is the JSON member called [name] in [json]. *)

val array : string -> Jsont.json -> Jsont.json list
(** [array name json] is the JSON array stored in the member called [name] in [json]. *)

val object_value : string -> Jsont.json -> Jsont.json
(** [object_value name json] is the JSON object stored in the member called [name] in
    [json]. *)

val string_value : string -> Jsont.json -> string
(** [string_value label json] is the string represented by [json]. *)

val number_value : string -> Jsont.json -> float
(** [number_value label json] is the number represented by [json]. *)

val check_string : string -> string -> Jsont.json -> unit
(** [check_string label expected json] checks that [json] represents [expected]. *)

val check_member_string : string -> string -> string -> Jsont.json -> unit
(** [check_member_string label name expected json] checks that the member called [name] in
    [json] represents [expected]. *)

val check_member_number : string -> string -> float -> Jsont.json -> unit
(** [check_member_number label name expected json] checks that the member called [name] in
    [json] represents [expected]. *)

val check_bool_member : string -> string -> bool -> Jsont.json -> unit
(** [check_bool_member label name expected json] checks that the member called [name] in
    [json] represents [expected]. *)

val check_string_array : string -> string list -> Jsont.json -> unit
(** [check_string_array label expected json] checks that [json] is an array containing
    [expected]. *)

val read_fixture : fixture_path:string -> unit -> string
(** [read_fixture ~fixture_path ()] is the contents of the fixture at [fixture_path]. *)

val expect_parts : label:string -> Stream_part.t list -> Stream_part.t list -> unit
(** [expect_parts ~label expected observed] checks that [observed] is [expected]. *)
