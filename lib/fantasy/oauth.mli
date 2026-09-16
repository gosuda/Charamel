(** Anthropic OAuth login, token exchange, refresh, and freshness management.

    [begin_login] builds the authorization URL for a browser login. The redirect is
    checked against the pending login before [exchange] sends the authorization code.
    Refresh operations preserve the stored account identifier. *)

module Credential : sig
  type t = {
    access : string;
    refresh : string;
    expires_at_ms : int;
    account : string option;
  }
  (** The type for a stored credential.

      [access] is the bearer token. [refresh] is the rotation token. [expires_at_ms] is
      the wall-clock millisecond timestamp after which the access token is stale. It
      includes a five-minute safety shave. [account] is the account identifier reported by
      the token endpoint. *)

  val pp : t Fmt.t
  (** [pp] formats a credential without revealing its tokens. *)
end

type error = Error.t
(** The type for OAuth, HTTP, and transport failures. *)

val pp_error : error Fmt.t
(** [pp_error] formats an OAuth, HTTP, or transport failure without revealing credentials.
*)

module Anthropic : sig
  val client_id : string
  (** [client_id] is the public Claude CLI OAuth client identifier. *)

  val authorize_url : string
  (** [authorize_url] is the browser-facing authorization endpoint. *)

  val token_url : string
  (** [token_url] is the token exchange and refresh endpoint. *)

  val scopes : string list
  (** [scopes] are the requested scopes in request order. *)

  type login = { pkce : Pkce.t; state : string; uri : string }
  (** The type for a pending login.

      [pkce] holds the verifier to present at exchange time. [state] is the anti-forgery
      value to confirm on the redirect. [uri] is the URL to open in a browser. *)

  val begin_login : ?rng:(int -> string) -> redirect_uri:string -> unit -> login
  (** [begin_login ?rng ~redirect_uri ()] starts a login.

      The state is 32 hexadecimal characters drawn from [rng 16]. [rng] defaults to
      [Mirage_crypto_rng.generate]. The PKCE verifier is base64url encoding of 96 entropy
      bytes. The URL carries [response_type=code], [client_id], [redirect_uri], the
      space-joined [scope], [code_challenge], [code_challenge_method=S256], [state], and
      the literal [code=true].

      @raise Invalid_argument
        if [redirect_uri] is empty or the entropy source returns an unexpected length. *)

  val extract_code : url_or_code:string -> state:string -> (string, error) result
  (** [extract_code ~url_or_code ~state] extracts an authorization code from a bare code,
      a [code#state] pair, or a full redirect URL. A redirect URL must contain exactly one
      matching [state] parameter and one nonempty [code] parameter. *)

  val compute_expires_at_ms : int -> int -> int
  (** [compute_expires_at_ms now_ms expires_in] is the expiry timestamp after subtracting
      the five-minute safety shave from [expires_in] seconds.

      @raise Invalid_argument
        if [now_ms] is negative, [expires_in] is not positive, or the timestamp would
        overflow an integer. *)

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
  (** [exchange ?now_ms ~sw ~clock ~net ~redirect_uri ~login ~code ()] swaps an
      authorization code for a credential.

      The request carries [grant_type=authorization_code], the client id, redirect URI,
      PKCE verifier, and state. The redirect URI must match the pending login. The token
      endpoint is contacted over validated TLS with a finite deadline and a bounded
      response body. [now_ms] defaults to the supplied clock. *)

  val refresh :
    ?now_ms:int ->
    sw:Eio.Switch.t ->
    clock:_ Eio.Time.clock ->
    net:_ Eio.Net.t ->
    Credential.t ->
    (Credential.t, error) result
  (** [refresh ?now_ms ~sw ~clock ~net credential] rotates [credential].

      The request carries the Claude Code OAuth beta header and SDK user agent. The stored
      account identifier is preserved. [now_ms] defaults to the supplied clock. *)

  val ensure_fresh :
    sw:Eio.Switch.t ->
    clock:_ Eio.Time.clock ->
    net:_ Eio.Net.t ->
    Credential.t ->
    (Credential.t, error) result
  (** [ensure_fresh ~sw ~clock ~net credential] refreshes [credential] when it expires
      within 60 seconds. Otherwise it returns [credential] unchanged. *)
end
