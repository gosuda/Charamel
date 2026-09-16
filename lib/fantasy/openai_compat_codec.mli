(** OpenAI-compatible Chat Completions codec.

    The codec is one side of the private boundary described by the fantasy codec contract:
    [encode] turns a {!Request.t} into the JSON body of one [POST <base>/chat/completions]
    call with [stream: true], and the decoder turns the SSE events of that call into
    {!Stream_part.t} values. It owns no transport, no credentials and no retries.

    The wire is the OpenAI chat completions stream shared by openai, openrouter, groq,
    ollama, mistral, xai, deepseek, cerebras, lm studio and hyper. Every event is one
    chunk of a [chat.completion.chunk] object; a final chunk carries [usage] and the
    sentinel [data: [DONE]] closes the stream. Because [usage] arrives on chunks of its
    own after the chunk naming [finish_reason], the decoder keeps the terminal
    {!Stream_part.Finish} back until the stream ends.

    Tool calls arrive fragmented: one [tool_calls] entry opens a call with an [index], an
    [id] and a [function.name], later entries with the same index append argument bytes.
    The decoder accumulates by index and attributes every delta to the id of the call the
    index opened, so interleaved parallel calls keep their identity. DeepSeek/Kimi style
    [reasoning_content] fields map onto {!Stream_part.Reasoning_delta}.

    Source: [.references/fantasy/providers/openai/language_model.go], function [Stream]
    (453-771) for chunk decoding and the terminal evaluation, [prepareParams] (259-363)
    for the request, [DefaultMapFinishReasonFunc] and [DefaultStreamUsageFunc] in
    [providers/openai/language_model_hooks.go] for the two mappings;
    [providers/openaicompat/language_model_hooks.go] [ToPromptFunc] (189-566) and
    [StreamExtraFunc] (106-184) for reasoning replay and reasoning detection;
    [providers/openaicompat/replay_test.go] for the DeepSeek/Kimi chunk shapes. *)

val encode : Request.t -> Jsont.json
(** [encode r] is the request body for [r] on [POST /chat/completions].

    The body carries [model], [stream: true], [max_tokens], [reasoning_effort] when
    [r.reasoning] is not [Off], [temperature] when set and reasoning is off, the
    conversation as [messages] and offered tools as [tools]. [File] parts map onto
    [image_url], [input_audio] and [file] content parts; a [Tool_result] of one [Tool]
    message maps onto one [role: "tool"] message per result. Assistant reasoning replays
    as the [reasoning_content] member, concatenated in order, on the same message as its
    text and tool calls. *)

type t
(** The type for incremental stream decoders. A decoder holds the tool calls opened so far
    keyed by chunk index, the reasoning block state per index, token usage accumulated
    from the stream, the pending finish reason, and whether the stream has ended. *)

val create : unit -> t
(** [create ()] is a decoder for one stream. *)

val feed : t -> event:string -> data:string -> Stream_part.t list
(** [feed t ~event ~data] is the parts produced by one complete server-sent event. [event]
    is the value of the SSE [event] field; an unnamed event is commonly delivered as
    ["message"] (or [""]) and then uses the [type] member of [data]. [data] is the joined
    payload of the event's [data] lines.

    Chat completions streams carry unnamed events only; a named event other than the
    [type]-named chunk is ignored. Text, reasoning and tool input deltas are emitted as
    they arrive. A [finish_reason] on a choice is recorded, not emitted: the terminal
    [Finish] is deferred so the trailing [usage] chunk is never lost. Malformed JSON, a
    payload that is not a chunk object, a [tool_calls] entry with an unknown call type,
    and an error object in the stream each produce one [Finish (`Error _)] and the decoder
    emits nothing afterwards. *)

val finish : t -> Stream_part.t list
(** [finish t] ends [t] at a clean end of response, after the [data: [DONE]] sentinel or
    at raw HTTP end of file.

    It emits any usage not yet emitted followed by the deferred terminal: the mapped
    [finish_reason], [`Tool_calls] when the stream named no finish reason but every
    accumulated call parsed as complete JSON, and [Finish (`Error _)] when the stream was
    cut, arguments were truncated, or no provider finish ever arrived — never a fake
    success. After a terminal [Finish] this is [[]]. *)
