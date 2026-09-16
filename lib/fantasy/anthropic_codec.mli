(** Anthropic Messages API request encoding and stream decoding.

    [encode] turns a request into the JSON body of a [POST /v1/messages] call. The decoder
    consumes one server-sent event at a time through [feed] and completes the stream
    through [finish]. Neither function reads HTTP, so a caller owns transport and headers.
*)

val encode : Request.t -> Jsont.json
(** [encode r] is the request body for [r] on the Anthropic Messages API. The body carries
    [stream: true]. [system] is a list of text blocks and carries [cache_control] on its
    last block. When [r.auth] is [Oauth] the Claude Code identification block is prepended
    as the first system text. The last two user turns carry [cache_control] on their last
    content block. [thinking] is present when [r.reasoning] is not [Off], with a budget of
    1024 for [Low], 8192 for [Medium] and 32768 for [High], clamped to [r.max_tokens - 1]
    and omitted entirely when that clamp is not positive. [temperature] is omitted when
    [thinking] is present, because the API rejects both together. Consecutive turns of the
    same role are merged into one message, because the API rejects two adjacent messages
    of the same role. *)

type t
(** The type for incremental stream decoders. A decoder holds the content block indices
    opened so far, the latest cumulative token usage snapshot reported by the stream, the
    stop reason of the last [message_delta] event, and whether a terminal event has been
    emitted. *)

val create : unit -> t
(** [create ()] is a decoder for one stream. *)

val feed : t -> event:string -> data:string -> Stream_part.t list
(** [feed t ~event ~data] is the parts produced by one complete server-sent event. [event]
    is the value of the [event] field; an unnamed SSE event is commonly delivered as
    ["message"] (or [""]) and then uses the [type] member of [data]. [data] is the joined
    payload.

    Text, thinking and tool input deltas are emitted as they arrive. Tool input deltas are
    attributed to the tool use block whose [content_block_start] opened the same index, so
    interleaved blocks keep their identity. Usage is emitted once, at the first event that
    reports token counts and at [message_delta] in particular. [Finish] is emitted once,
    at [message_stop], at an [error] event, or when [data] is not JSON. Events the
    contract does not model, such as [ping], and deltas with no counterpart in
    [Stream_part.t], such as [signature_delta], produce no parts.

    After a terminal part, and after any part that ends the stream, [feed] returns the
    empty list. *)

val finish : t -> Stream_part.t list
(** [finish t] ends [t] at a clean end of response. It emits any token usage not yet
    emitted followed by [Finish (`Error _)], because a stream that closed before
    [message_stop] is incomplete rather than successful. It returns the empty list when
    [t] already produced a terminal part. *)
