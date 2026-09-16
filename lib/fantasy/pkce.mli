(** PKCE code verifier and challenge pairs for OAuth flows.

    A pair contains a high-entropy verifier and its derived S256 challenge. Authorization
    requests carry the challenge. Token exchanges prove possession with the verifier. *)

type t = { verifier : string; challenge : string }
(** The type for a PKCE pair. [verifier] is base64url-encoded entropy and [challenge] is
    the unpadded base64url-encoded SHA-256 digest of the verifier. *)

val of_verifier : string -> t
(** [of_verifier verifier] is the PKCE pair derived from [verifier].

    [verifier] must contain 43 to 128 RFC 7636 unreserved characters.

    @raise Invalid_argument if [verifier] is outside the RFC 7636 limits. *)

val generate : ?rng:(int -> string) -> unit -> t
(** [generate ?rng ()] draws 96 bytes of entropy with [rng], encodes the bytes as the
    verifier, and derives the S256 challenge.

    [rng n] returns [n] bytes of entropy. [rng] defaults to [Mirage_crypto_rng.generate].

    @raise Invalid_argument if [rng] returns a string of the wrong length. *)
