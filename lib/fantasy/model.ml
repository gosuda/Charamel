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

let jsont ~unknown =
  let open Jsont in
  let base =
    Object.map ~kind:"model"
      (fun
        id
        name
        cost_in
        cost_out
        cost_cache_write
        cost_cache_read
        context_window
        default_max_tokens
        can_reason
        supports_attachments
      ->
        {
          id;
          name;
          provider = "";
          context_window;
          default_max_tokens;
          can_reason;
          supports_attachments;
          cost_in;
          cost_out;
          cost_cache_read;
          cost_cache_write;
        })
    |> Object.mem "id" string ~enc:(fun m -> m.id)
    |> Object.mem "name" string ~enc:(fun m -> m.name)
    |> Object.mem "cost_per_1m_in" number ~enc:(fun m -> m.cost_in)
    |> Object.mem "cost_per_1m_out" number ~enc:(fun m -> m.cost_out)
    |> Object.mem "cost_per_1m_in_cached" number ~dec_absent:0.0 ~enc:(fun m ->
        m.cost_cache_write)
    |> Object.mem "cost_per_1m_out_cached" number ~dec_absent:0.0 ~enc:(fun m ->
        m.cost_cache_read)
    |> Object.mem "context_window" int ~enc:(fun m -> m.context_window)
    |> Object.mem "default_max_tokens" int ~enc:(fun m -> m.default_max_tokens)
    |> Object.mem "can_reason" bool ~dec_absent:false ~enc:(fun m -> m.can_reason)
    |> Object.mem "supports_attachments" bool ~dec_absent:false ~enc:(fun m ->
        m.supports_attachments)
  in
  match unknown with
  | `Error -> base |> Object.error_unknown |> Object.finish
  | `Skip -> base |> Object.finish
