(** A model offered by a provider.

    [t] is the catalog-level description a caller needs to pick a model and price a
    session: identifiers, context and output ceilings, capability flags, and
    per-million-token costs. *)

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
(** The type for a model.

    [id] is the provider's model identifier, [name] a display name, [provider] the owning
    provider id, [context_window] the maximum prompt window in tokens,
    [default_max_tokens] the default output cap, [can_reason] whether the model exposes
    reasoning, and [supports_attachments] whether file parts are accepted. Costs are USD
    per million tokens; the cache costs are zero when the provider does not report them.
*)

val jsont : unknown:[ `Error | `Skip ] -> t Jsont.t
(** [jsont ~unknown] decodes and encodes a model entry.

    Wire members: [id], [name], [cost_per_1m_in], [cost_per_1m_out],
    [cost_per_1m_in_cached], [cost_per_1m_out_cached], [context_window],
    [default_max_tokens], [can_reason], and [supports_attachments]; the booleans and cache
    costs default to their zero values when absent. Unknown members are skipped when
    [unknown] is [`Skip] and rejected when it is [`Error]. The wire model carries no
    provider member, so [provider] decodes to ["" ] and is stamped by the enclosing
    provider codec. The cache costs cross on the way in: [cost_per_1m_in_cached] prices
    cache creation and lands in [cost_cache_write], while [cost_per_1m_out_cached] prices
    cache reads and lands in [cost_cache_read] — the accounting crush's agent performs
    with [CostPer1MInCached] on creation tokens and [CostPer1MOutCached] on read tokens.
*)
