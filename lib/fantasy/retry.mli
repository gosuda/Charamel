(** Retry policy for provider calls.

    [t] configures exponential backoff with jitter. Delays respect provider [Retry-After]
    and [retry-after-ms] headers when they name a sane window, and only retryable failures
    — timeouts, conflicts, rate limits, server errors, and transport failures — are
    retried. *)

type t = private {
  max : int;
  base : float;
  factor : float;
  max_delay : float;
  jitter : float;
}
(** The type for a retry policy.

    [max] is the number of retries after the first attempt, [base] the first delay in
    seconds, [factor] the multiplier per attempt, [max_delay] the delay ceiling in
    seconds, and [jitter] the fraction of randomisation applied to each computed delay. *)

val default : t
(** [default] is the approved plan policy: 8 retries, 0.5 s base, factor 2, 60 s ceiling,
    jitter 0.25. *)

val retryable_status : int -> bool
(** [retryable_status code] is [true] for 408, 409, 429 and any 5xx — the statuses a
    caller may re-issue as-is. *)

val delay :
  ?rng:(unit -> float) ->
  ?now:float ->
  t ->
  attempt:int ->
  retry_after:(string * string) list ->
  float
(** [delay ?rng ?now t ~attempt ~retry_after] computes the wait in seconds before retry
    number [attempt] (the first retry is [1]).

    [retry_after] holds the response's [Retry-After] and [retry-after-ms] headers as
    name/value pairs. A non-negative numeric delay, a valid HTTP-date delay relative to
    [now], or a millisecond delay within [max_delay] overrides exponential backoff. [now]
    is the current wall-clock time in seconds since the epoch and defaults to [0.].
    Otherwise the delay is [base * factor^(attempt-1)] capped at [max_delay], scaled by a
    uniform factor of [1 - jitter] to [1 + jitter]. [rng] draws the jitter sample and
    defaults to a [Mirage_crypto_rng] draw. *)
