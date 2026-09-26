(** Request vocabulary shared by the two OpenAI-family codecs.

    Each codec keeps its provider's event dispatch, member names, error envelope, and
    finish-reason vocabulary to itself; this module holds only the spelling the
    [chat/completions] and [responses] APIs agree on. *)

val effort : Request.reasoning -> string option
(** [effort level] is the provider effort string [low], [medium], or [high], and [None]
    for [Off]. *)
