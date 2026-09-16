type t = { id : string; name : string; base_url : string; models : Model.t list }

(* [model_codec] decodes a wire model with an empty provider and
   [jsont] stamps the owning provider id onto what it decodes. The
   cache costs cross on the way in: [cost_per_1m_in_cached] prices
   cache creation and lands in [Model.cost_cache_write], while
   [cost_per_1m_out_cached] prices cache reads and lands in
   [Model.cost_cache_read] — the accounting crush's agent performs
   with [CostPer1MInCached] on creation tokens and [CostPer1MOutCached]
   on read tokens. *)
let model_codec =
  let open Jsont in
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
        Model.id;
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
  |> Object.mem "id" string ~enc:(fun m -> m.Model.id)
  |> Object.mem "name" string ~enc:(fun m -> m.Model.name)
  |> Object.mem "cost_per_1m_in" number ~enc:(fun m -> m.Model.cost_in)
  |> Object.mem "cost_per_1m_out" number ~enc:(fun m -> m.Model.cost_out)
  |> Object.mem "cost_per_1m_in_cached" number ~dec_absent:0.0 ~enc:(fun m ->
      m.Model.cost_cache_write)
  |> Object.mem "cost_per_1m_out_cached" number ~dec_absent:0.0 ~enc:(fun m ->
      m.Model.cost_cache_read)
  |> Object.mem "context_window" int ~enc:(fun m -> m.Model.context_window)
  |> Object.mem "default_max_tokens" int ~enc:(fun m -> m.Model.default_max_tokens)
  |> Object.mem "can_reason" bool ~dec_absent:false ~enc:(fun m -> m.Model.can_reason)
  |> Object.mem "supports_attachments" bool ~dec_absent:false ~enc:(fun m ->
      m.Model.supports_attachments)
  |> Object.finish

let with_provider provider models =
  List.map (fun (m : Model.t) -> { m with Model.provider }) models

let jsont =
  let open Jsont in
  Object.map ~kind:"provider" (fun id name api_endpoint models ->
      { id; name; base_url = api_endpoint; models = with_provider id models })
  |> Object.mem "id" string ~enc:(fun p -> p.id)
  |> Object.mem "name" string ~enc:(fun p -> p.name)
  |> Object.mem "api_endpoint" string ~dec_absent:"" ~enc:(fun p -> p.base_url)
  |> Object.mem "models" (list model_codec) ~dec_absent:[] ~enc:(fun p -> p.models)
  |> Object.finish

let pp ppf { id; base_url; models; _ } =
  Fmt.pf ppf "%s (%s): %d models" id base_url (List.length models)
