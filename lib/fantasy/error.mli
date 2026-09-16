(** Shared provider errors.

    [t] is the closed failure surface for provider calls, catalog refreshes, and OAuth
    flows. Recoverable failures travel as [(result, error)] pairs; only programming errors
    raise. *)

type http_error = { status : int; title : string; message : string; retryable : bool }
(** The type for a failed HTTP response: [status] code, [title] and [message] diagnostics,
    and [retryable] per the retry rules. *)

type t =
  [ `Oauth of string
  | `Oauth_invalid_grant of string
  | `Http of http_error
  | `Transport of string ]
(** The type for provider failures.

    [`Oauth] covers OAuth failures that do not identify an invalid refresh grant.
    [`Oauth_invalid_grant] identifies a rejected or expired refresh grant, so callers can
    disable that credential. [`Http] carries a provider response, and [`Transport] a
    connection-level failure. *)

val pp : t Fmt.t
(** [pp] formats the failure for diagnostics. *)

val message : t -> string
(** [message e] is the human-readable failure text without the constructor label. *)
