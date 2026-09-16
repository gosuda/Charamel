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

val pp : t Fmt.t
(** [pp] formats id, provider, and window/cost summary. *)
