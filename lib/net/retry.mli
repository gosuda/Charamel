(** Retry policy for HTTP attempts.

    [t] configures exponential backoff with jitter. A caller-supplied [retry_after] delay
    — the value a [Retry-After] or [retry-after-ms] response header resolved to —
    overrides backoff whenever it names a window within [max_delay]. *)

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

val policy : max:int -> base:float -> factor:float -> max_delay:float -> jitter:float -> t
(** [policy ~max ~base ~factor ~max_delay ~jitter] validates and returns a policy. [max]
    must be non-negative, [base] and [max_delay] non-negative and finite, [factor] at
    least [1.], and [jitter] within \[0., 1.\]; otherwise [Invalid_argument]. *)

val default : t
(** [default] is the tree's policy: 8 retries, 0.5 s base, factor 2, 60 s ceiling, jitter
    0.25. *)

val retryable_status : int -> bool
(** [retryable_status code] is [true] for 408, 409, 429 and any 5xx — the statuses a
    caller may re-issue as-is. *)

val delay : ?rng:(unit -> float) -> t -> attempt:int -> retry_after:float option -> float
(** [delay ?rng t ~attempt ~retry_after] is the wait in seconds before retry number
    [attempt] (the first retry is [1]); [Invalid_argument] when [attempt] is not positive.
    A finite, non-negative [retry_after] within [max_delay] wins over backoff. Otherwise
    the delay is [base * factor^(attempt-1)] capped at [max_delay], scaled by a uniform
    factor of [1 - jitter] to [1 + jitter]. [rng] draws the jitter sample in \[0., 1.\]
    and defaults to the standard-library PRNG; a draw outside that range is
    [Invalid_argument]. *)
