type t = {
  input : int;
  output : int;
  cache_read : int;
  cache_write : int;
  reasoning : int;
}

let zero = { input = 0; output = 0; cache_read = 0; cache_write = 0; reasoning = 0 }

let add u v =
  {
    input = u.input + v.input;
    output = u.output + v.output;
    cache_read = u.cache_read + v.cache_read;
    cache_write = u.cache_write + v.cache_write;
    reasoning = u.reasoning + v.reasoning;
  }

let total u = u.input + u.output + u.cache_read + u.cache_write

let pp ppf u =
  Fmt.pf ppf "in=%d out=%d cache r/w=%d/%d reasoning=%d" u.input u.output u.cache_read
    u.cache_write u.reasoning
