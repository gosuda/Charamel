type t = {
  id : string;
  name : string;
  provider : string;
  context_window : int;
  default_max_tokens : int;
  can_reason : bool;
  supports_attachments : bool;
  cost_in : float;
  cost_out : float;
  cost_cache_read : float;
  cost_cache_write : float;
}

let pp ppf m =
  Fmt.pf ppf "@[<h>%s (%s) window=%d max=%d in=%.2f out=%.2f@]" m.id m.provider
    m.context_window m.default_max_tokens m.cost_in m.cost_out
