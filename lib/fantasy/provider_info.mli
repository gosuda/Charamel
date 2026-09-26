(** One provider entry of the catwalk model catalog.

    An entry names a provider, the endpoint it serves, and the models it offers with the
    window, capability, and pricing metadata a caller needs to pick a model and price a
    session. *)

type t = { id : string; name : string; base_url : string; models : Model.t list }
(** The type for a catalog entry.

    [id] is the provider identifier, also carried by each of [models] in [Model.provider],
    [name] the display name, and [models] the served models in catalog order.

    [base_url] is the default API endpoint, verbatim from the catalog: a value of the form
    ["$VAR"] names the environment variable holding the endpoint and must be resolved by
    the caller; the catalog carries no environment. *)

val jsont : t Jsont.t
(** [jsont] decodes and encodes one element of the catwalk [/v2/providers] array.

    Members the catalog does not keep — [api_key], [type], [default_headers],
    [default_large_model_id], [default_small_model_id] on the provider, and
    [reasoning_levels], [default_reasoning_effort] and [options] on a model — are skipped,
    so a catalog the upstream service extends still decodes. Optional members that are
    absent decode to catwalk's Go zero values: [base_url] to [""] and the booleans and
    cache costs to [false] and [0.0]. The wire model carries no provider member, so
    [Model.provider] is stamped from the enclosing entry's [id] and dropped again on
    encode: a decoded entry re-encodes unchanged. *)
