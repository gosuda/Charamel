type t = { max : int; base : float; factor : float; max_delay : float; jitter : float }

let policy ~max ~base ~factor ~max_delay ~jitter =
  if
    max < 0
    || (not (Float.is_finite base))
    || base < 0.
    || (not (Float.is_finite factor))
    || factor < 1.
    || (not (Float.is_finite max_delay))
    || max_delay < 0.
    || (not (Float.is_finite jitter))
    || jitter < 0. || jitter > 1.
  then invalid_arg "Charamel_net.Retry.policy: invalid retry bounds";
  { max; base; factor; max_delay; jitter }

let default = policy ~max:8 ~base:0.5 ~factor:2. ~max_delay:60. ~jitter:0.25

let retryable_status = function
  | 408 | 409 | 429 -> true
  | status -> status >= 500 && status <= 599

let uniform () = float_of_int (Random.int 1_000_000) /. 1_000_000.

let delay ?(rng = uniform) t ~attempt ~retry_after =
  if attempt < 1 then invalid_arg "Charamel_net.Retry.delay: attempt must be positive";
  match retry_after with
  | Some seconds when Float.is_finite seconds && seconds >= 0. && seconds <= t.max_delay
    ->
      seconds
  | _ ->
      let backoff =
        Float.min t.max_delay (t.base *. Float.pow t.factor (float_of_int (attempt - 1)))
      in
      let draw = rng () in
      if (not (Float.is_finite draw)) || draw < 0. || draw > 1. then
        invalid_arg "Charamel_net.Retry.delay: rng outside [0,1]";
      Float.min t.max_delay (backoff *. (1. -. t.jitter +. (2. *. t.jitter *. draw)))
