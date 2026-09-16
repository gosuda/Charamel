(** Token usage accounting for a model call.

    [t] carries the token counts a provider reports for one request: prompt tokens,
    completion tokens, prompt-cache reads and writes, and reasoning tokens where the
    provider bills them separately. Counters are totalled per stream by the providers;
    [zero] is the additive identity. *)

type t = {
  input : int;
  output : int;
  cache_read : int;
  cache_write : int;
  reasoning : int;
}
(** The type for usage.

    [input] is the number of ordinary prompt tokens, excluding cache reads and cache
    writes. [output] is the number of billed completion tokens, including any reasoning
    tokens the provider bills as output. [cache_read] and [cache_write] are the prompt
    tokens served from, and written to, a prompt cache: separate, non-overlapping prompt
    buckets from [input]. [reasoning] is the informational reported subset of [output]
    spent on hidden reasoning; it is never added again to a total. The effective prompt
    size is [input + cache_read + cache_write]. *)

val zero : t
(** [zero] is the usage with every counter at [0]. *)

val add : t -> t -> t
(** [add u v] is the pointwise sum of [u] and [v]. Accumulators fold per-request usage
    with [add]. *)

val total : t -> int
(** [total u] is [input u + cache_read u + cache_write u + output u] (full accounted
    tokens). [reasoning] is excluded from the sum because it is already counted inside
    [output]. *)

val pp : t Fmt.t
(** [pp] formats usage as [in=N out=N cache r/w=N/N reasoning=N]. *)
