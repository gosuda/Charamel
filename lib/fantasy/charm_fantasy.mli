(** Charm's LLM provider SDK.

    [Charm_fantasy] speaks to Anthropic, OpenAI-compatible, OpenAI Responses, and Google
    endpoints. A provider is constructed from a base URL and an authentication mode;
    [Provider.stream] drives one request and yields the seven approved event kinds into a
    bounded Eio stream ending with exactly one terminal [Finish]. *)

module Error = Error
(** Shared provider errors. *)

module Message = Message
(** Conversation messages and their typed parts. *)

module Tool = Tool
(** Tool definitions offered to the model. *)

module Usage = Usage
(** Token usage accounting. *)

module Model = Model
(** Model catalog entries. *)

module Stream_part = Stream_part
(** The seven streaming event kinds. *)

module Retry = Retry
(** Exponential-backoff retry policy. *)

module Pkce = Pkce
(** PKCE code verifier/challenge pairs. *)

module Oauth : sig
  module Credential = Oauth.Credential
  (** Stored credentials with token-redacting diagnostics. *)

  type error = Error.t
  (** The type for OAuth, HTTP, and transport failures. *)

  val pp_error : error Fmt.t
  (** [pp_error] formats an OAuth failure without revealing credentials. *)

  module Anthropic : sig
    val client_id : string
    (** [client_id] is the public Claude CLI OAuth client identifier. *)

    val authorize_url : string
    (** [authorize_url] is the browser authorization endpoint. *)

    val token_url : string
    (** [token_url] is the token exchange and refresh endpoint. *)

    val scopes : string list
    (** [scopes] is the requested scope list in request order. *)

    type login = Oauth.Anthropic.login = { pkce : Pkce.t; state : string; uri : string }
    (** The type for a pending login and its authorization URL. *)

    val begin_login : ?rng:(int -> string) -> redirect_uri:string -> unit -> login
    (** [begin_login ?rng ~redirect_uri ()] is a pending login with a fresh PKCE verifier
        and state. [rng] defaults to [Mirage_crypto_rng.generate].

        @raise Invalid_argument
          if [redirect_uri] is empty or [rng] returns an unexpected number of bytes. *)

    val extract_code : url_or_code:string -> state:string -> (string, error) result
    (** [extract_code ~url_or_code ~state] is the authorization code from a bare code, a
        [code#state] pair, or a redirect URL with matching state. *)

    val exchange :
      ?now_ms:int ->
      sw:Eio.Switch.t ->
      clock:_ Eio.Time.clock ->
      net:_ Eio.Net.t ->
      redirect_uri:string ->
      login:login ->
      code:string ->
      unit ->
      (Credential.t, error) result
    (** [exchange ?now_ms ~sw ~clock ~net ~redirect_uri ~login ~code ()] exchanges [code]
        for a credential. [redirect_uri] must match the pending login. [now_ms] defaults
        to the supplied clock in milliseconds. *)

    val refresh :
      ?now_ms:int ->
      sw:Eio.Switch.t ->
      clock:_ Eio.Time.clock ->
      net:_ Eio.Net.t ->
      Credential.t ->
      (Credential.t, error) result
    (** [refresh ~sw ~clock ~net credential] rotates [credential] and preserves its
        account identifier. [now_ms] defaults to the supplied clock in milliseconds. *)

    val ensure_fresh :
      sw:Eio.Switch.t ->
      clock:_ Eio.Time.clock ->
      net:_ Eio.Net.t ->
      Credential.t ->
      (Credential.t, error) result
    (** [ensure_fresh ~sw ~clock ~net credential] refreshes [credential] if its expiry is
        within 60 seconds. Otherwise it is [Ok credential]. *)
  end
end

module Catalog = Catalog
(** The vendored provider catalog with etag refresh. *)

module Provider = Provider
(** The four provider constructors and the streaming entry point. *)

module Provider_info = Provider_info
(** Catalog provider records. *)
