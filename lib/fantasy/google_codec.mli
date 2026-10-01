(** Google Gemini [streamGenerateContent] codec.

    The codec is one side of the private boundary described by the fantasy codec contract:
    [encode] turns a {!Request.t} into the JSON body of one
    [POST .../models/<id>:streamGenerateContent?alt=sse] call and the decoder turns the
    SSE events of that call into {!Stream_part.t} values. It owns no transport, no
    credentials and no retries.

    Wire shape: every event is a complete [GenerateContentResponse] holding one or more
    [candidates], optional [usageMetadata], and optional [promptFeedback]. Because usage
    can arrive on events after the one that carries [finishReason], the decoder keeps the
    terminal {!Stream_part.Finish} back until the stream ends.

    Usage: [usageMetadata] is a cumulative snapshot, so the last one wins and is emitted
    once, immediately before the terminal. [output] is [candidatesTokenCount] plus
    [thoughtsTokenCount]: upstream's [mapUsage] keeps the two disjoint, but [Usage.t]
    bills [reasoning] as a subset of [output], so the fold is required by this contract,
    not by the wire.

    Source: [.references/fantasy/providers/google/google.go], function [prepareParams] for
    the request and [languageModel.Stream] for the response; [mapUsage] and
    [mapFinishReason] for the two mappings; recorded responses under
    [.references/fantasy/providers/providertests/testdata/TestGoogle*/]. *)

val encode : Request.t -> Jsont.json
(** [encode r] is the request body for [r].

    The body carries [systemInstruction] when [Request.system r] is non-empty, [contents]
    built from [Request.messages r], [tools] as a single [functionDeclarations] entry when
    tools are offered, and [generationConfig] with [maxOutputTokens], [temperature] and,
    when [Request.reasoning r] is not [Off], a [thinkingConfig]. *)

type t
(** The type for the incremental decoder state. *)

val create : unit -> t
(** [create ()] is a decoder expecting the first event of a stream. *)

val feed : t -> event:string -> data:string -> Stream_part.t list
(** [feed d ~event ~data] decodes one complete SSE event.

    [event] is the SSE event name, [data] the joined payload of its [data:] lines. Events
    other than the unnamed [message] event are ignored.

    A [promptFeedback.blockReason] ends the stream with the content-filter terminal even
    when the response carries no candidates. Malformed JSON, a JSON payload that is not an
    object, and a truncated stream signalled by the provider all produce one
    [Finish (`Error _)] and the decoder emits nothing afterwards. *)

val finish : t -> Stream_part.t list
(** [finish d] ends [d] at HTTP end of file.

    It emits the last [usageMetadata] snapshot the stream reported, then the deferred
    [Stream_part.Finish], and when no provider finish arrived it produces
    [Finish (`Error _)] instead of a fake success. After a terminal [Finish] this is [[]].
*)
