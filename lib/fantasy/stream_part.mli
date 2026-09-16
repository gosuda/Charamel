(** One event delivered by a streaming model call.

    [t] is the provider-neutral event surface every codec maps onto: incremental text and
    reasoning deltas, tool calls assembled from input deltas, token usage, and the single
    terminal [Finish]. *)

type t =
  | Text_delta of string
  | Reasoning_delta of string
  | Tool_call_start of { id : string; name : string }
  | Tool_input_delta of { id : string; delta : string }
  | Tool_call_end of string
  | Usage of Usage.t
  | Finish of [ `Stop | `Tool_calls | `Length | `Content_filter | `Error of string ]

(** The type for a stream event.

    [Text_delta] and [Reasoning_delta] carry incremental text; [Tool_call_start] opens a
    call identified by [id], [Tool_input_delta] appends argument bytes to that [id], and
    [Tool_call_end] closes it, returning the [id]. [Usage] reports token counts. [Finish]
    is terminal: [`Tool_calls] means the model requests tool execution, [`Length] that
    output was truncated by the token cap, [`Content_filter] that safety machinery stopped
    the turn, and [`Error] that the provider ended with a failure message. A successful
    stream ends with exactly one [Finish]; failure replaces it with [Finish (`Error _)].
*)

val pp : t Fmt.t
(** [pp] formats one event compactly for diagnostics. *)
