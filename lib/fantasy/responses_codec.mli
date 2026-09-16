(** OpenAI Responses API request encoding and stream decoding.

    [encode] turns a request into the JSON body of a [POST /v1/responses] call. The
    decoder consumes one server-sent event at a time through [feed] and completes the
    stream through [finish]. Neither function reads HTTP, so a caller owns transport and
    headers.

    [finish] defers the terminal {!Stream_part.Finish} to the end of the stream, because
    usage and the finish reason arrive on [response.completed] while tool calls may still
    stream after the last text; a premature end of file is an error, never a fake success.
*)

val encode : Request.t -> Jsont.json
(** [encode r] is the request body for [r] on the Responses API.

    The body carries [model], [input] built from [Request.messages r], [tools] and
    [tool_choice: "auto"] when tools are offered, [max_output_tokens], [temperature] when
    set, [stream: true] and [store: false]. When [Request.reasoning r] is not [Off] and
    the model is a reasoning model, [reasoning] carries [effort] [low], [medium] or
    [high], and never a [summary], because the provider only streams reasoning text when
    the caller explicitly asks for summaries. For a reasoning model [temperature] is
    dropped, because the API rejects both together. System text takes the [developer] role
    on a reasoning model and the [system] role otherwise, as a plain string content
    message, and is removed entirely for [o1-mini] and [o1-preview]. *)

type t
(** The type for incremental stream decoders. A decoder holds the tool calls opened so far
    keyed by output index, the token usage snapshot of the latest terminal event, the
    recorded finish reason, and whether a tool call completed on the stream. *)

val create : unit -> t
(** [create ()] is a decoder for one stream. *)

val feed : t -> event:string -> data:string -> Stream_part.t list
(** [feed t ~event ~data] is the parts produced by one complete server-sent event. [event]
    is the value of the [event] field and [data] the joined [data] payload; the event name
    is not used to dispatch, because every recorded stream carries the authoritative
    [type] member in [data].

    Text and reasoning summary deltas are emitted as they arrive. A [function_call] output
    item opens a tool call whose id is the item's [call_id], and
    [response.function_call_arguments.delta] events append to the call opened at the same
    output index, so interleaved calls keep their identity. A
    [response.reasoning_summary_part.added] event opens a new reasoning section with a
    newline delta. Usage is emitted once, immediately before the terminal part, from the
    [response.completed] or [response.incomplete] snapshot; the snapshot already reports
    cache and reasoning tokens separately, and [input_tokens] includes cached tokens, so
    [cache_read] is subtracted from [input] to keep the counters disjoint.

    [response.completed] ends the stream with the finish reason mapped from
    [incomplete_details.reason] and upgrades to [`Tool_calls] when a tool call completed
    on the stream. [response.incomplete] with reason [max_tokens] or [max_output_tokens]
    maps to [`Length] and [content_filter] maps to [`Content_filter]. [response.failed]
    and [error] end the stream with [Finish (`Error _)].

    A malformed [data] payload produces one [Finish (`Error _)] and the decoder emits
    nothing afterwards. Events the contract does not model, such as
    [response.in_progress], [response.created], [response.output_text.done] and [ping],
    produce no parts.

    After a terminal part, [feed] returns the empty list. *)

val finish : t -> Stream_part.t list
(** [finish t] ends [t] at a clean end of response. It emits the token usage of the last
    snapshot not yet emitted followed by the deferred [Finish], and when no
    [response.completed] or [response.incomplete] arrived it produces [Finish (`Error _)],
    because a stream that closed before its terminal event is incomplete rather than
    successful. After a terminal part this returns the empty list. *)
