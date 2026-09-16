(** The shared request record consumed by every provider codec.

    [t] carries the protocol inputs of one streaming call: the model, the authentication
    mode, system blocks, tools, sampling knobs, and the conversation. Credentials are not
    part of [t]; the provider resolves them from its [Provider.auth] and injects headers
    at the transport boundary. *)

type reasoning =
  | Off
  | Low
  | Medium
  | High
      (** The type for a reasoning effort level. Codecs map the level to provider-specific
          budgets or effort strings. *)

type auth =
  | Api_key
  | Oauth
      (** The type for the authentication mode of a request. [Oauth] selects provider
          OAuth wire fingerprints (bearer auth, extra headers). *)

type t = {
  model : Model.t;
  auth : auth;
  system : string list;
  tools : Tool.t list;
  max_tokens : int;
  temperature : float option;
  reasoning : reasoning;
  messages : Message.t list;
}
(** The type for one streaming request.

    [system] holds system blocks in order, [tools] the offered tools, [max_tokens] the
    output token cap, [temperature] the sampling temperature ([None] lets the provider
    default apply), and [reasoning] the requested reasoning level. [messages] is the
    conversation in order. *)

val of_call :
  model:Model.t ->
  auth:auth ->
  ?system:string list ->
  ?tools:Tool.t list ->
  ?max_tokens:int ->
  ?temperature:float ->
  ?reasoning:reasoning ->
  Message.t list ->
  t
(** [of_call ~model ~auth ?system ?tools ?max_tokens ?temperature ?reasoning messages]
    builds a request.

    [system] defaults to [[]], [tools] to [[]], [max_tokens] to the model's
    [default_max_tokens], [temperature] to [None], and [reasoning] to [Off]. *)
