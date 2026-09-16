(** The provider surface: constructors for the four shipped backends and the streaming
    entry point.

    A provider bundles base URL, authentication mode, and headers. Its [stream] resolves
    the request into a codec, drives the transport, and yields the seven approved stream
    events into a bounded Eio stream with exactly one terminal [Finish]. *)

type t
(** The type for a provider handle. *)

type auth =
  | Api_key of string
  | Oauth of Oauth.Credential.t
      (** The type for provider authentication: an API key, or an Anthropic OAuth
          credential. *)

val anthropic : ?base_url:string -> auth:auth -> unit -> t
(** [anthropic ?base_url ~auth ()] is the Anthropic Messages provider. [base_url] defaults
    to [https://api.anthropic.com]. *)

val openai_compatible :
  base_url:string -> ?headers:(string * string) list -> auth:auth -> unit -> t
(** [openai_compatible ~base_url ?headers ~auth ()] is the OpenAI Chat Completions
    provider for any compatible endpoint; [headers] ride along on every request. *)

val openai_responses : ?base_url:string -> auth:auth -> unit -> t
(** [openai_responses ?base_url ~auth ()] is the OpenAI Responses provider. [base_url]
    defaults to [https://api.openai.com/v1]. *)

val google : ?base_url:string -> auth:auth -> unit -> t
(** [google ?base_url ~auth ()] is the Google Generative Language provider. [base_url] is
    the origin and defaults to [https://generativelanguage.googleapis.com]; the streaming
    path [/v1beta/models/<id>:streamGenerateContent?alt=sse] is appended. *)

val stream :
  t ->
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  model:Model.t ->
  ?system:string list ->
  ?tools:Tool.t list ->
  ?max_tokens:int ->
  ?temperature:float ->
  ?reasoning:[ `Off | `Low | `Medium | `High ] ->
  ?on_error:(Error.t -> unit) ->
  Message.t list ->
  Stream_part.t Eio.Stream.t
(** [stream t ~sw ~clock ~net ~model ?system ?tools ?max_tokens ?temperature ?reasoning
     ?on_error messages] starts one streaming call and returns the bounded event stream.

    [system] defaults to [[]], [tools] to [[]], [max_tokens] to the model's default,
    [temperature] to the provider default, and [reasoning] to [`Off]. [on_error] defaults
    to ignoring typed HTTP, transport, and OAuth failures; when supplied it is called once
    per request before the corresponding terminal error. The returned stream ends with
    exactly one terminal [Finish]; failures surface as [Finish (`Error _)] after the
    transport's retry policy is exhausted. Cancellation of [sw] propagates as cancellation
    and is never retried. *)
