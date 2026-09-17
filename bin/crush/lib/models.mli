(** Model catalog loading, provider construction, and role selection.

    [Models] combines the embedded provider snapshot with the optional cache and project
    configuration. It never fabricates a provider or credential. *)

type resolved = {
  role : [ `Large | `Small ];
  provider_id : string;
  provider : Charamel_fantasy.Provider.t;
  model : Charamel_fantasy.Model.t;
  reasoning : Config.reasoning;
  max_tokens : int;
}
(** A fully resolved role, including the provider handle used for requests. *)

type error =
  [ `No_model of [ `Large | `Small ]
  | `Unknown_provider of string
  | `Unknown_model of string * string
  | `No_credential of string
  | `Disabled of string
  | `Auth of Auth.error ]
(** The type for model-selection failures. *)

val pp_error : error Fmt.t
(** [pp_error ppf error] formats a model-selection failure. *)

val catalog_cache_path : unit -> string
(** [catalog_cache_path ()] is the provider catalog cache path. *)

val catalog :
  fs:Eio.Fs.dir_ty Eio.Path.t -> Config.t -> Charamel_fantasy.Provider_info.t list
(** [catalog ~fs config] loads a parseable cached catalog, otherwise the embedded
    snapshot, and overlays configured providers and models. *)

val resolve :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  Config.t ->
  auth:Auth.t ->
  env:(string -> string option) ->
  role:[ `Large | `Small ] ->
  (resolved, error) result
(** [resolve ~fs config ~auth ~env ~role] resolves an explicit configured role, or chooses
    a deterministic default provider and model with a usable credential. *)

val with_auth :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  Config.t ->
  env:(string -> string option) ->
  resolved ->
  Charamel_fantasy.Provider.auth ->
  (resolved, error) result
(** [with_auth ~fs config ~env resolved provider_auth] rebuilds only the provider handle
    for [resolved] with [provider_auth]. The selected provider, model, role, reasoning,
    and token limit are preserved. *)

val cost : Charamel_fantasy.Model.t -> Charamel_fantasy.Usage.t -> float
(** [cost model usage] is the usage charge in US dollars. *)

val list :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  Config.t ->
  auth:Auth.t ->
  env:(string -> string option) ->
  (string * Charamel_fantasy.Model.t list * [ `Ready | `No_credential | `Disabled ]) list
(** [list ~fs config ~auth ~env] lists catalog models and credential state for every
    provider in catalog order. *)

type update_result =
  | Updated of string
  | Not_modified
      (** The result of refreshing the provider catalog. [Updated] carries the ETag
          returned by the endpoint, which may be empty. *)

type update_error =
  [ `Io of string * string | `Parse of string | `Fetch of Charamel_fantasy.Error.t ]
(** The type for catalog cache refresh failures. [Parse] carries a path followed by a
    diagnostic message. *)

val pp_update_error : update_error Fmt.t
(** [pp_update_error ppf error] formats a catalog cache refresh failure. *)

val save_catalog :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  etag:string ->
  Charamel_fantasy.Provider_info.t list ->
  (unit, update_error) result
(** [save_catalog ~fs ~etag providers] atomically writes the provider cache in its JSON
    object shape. *)

val update_catalog :
  ?source:string ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  net:Eio_unix.Net.t ->
  clock:_ Eio.Time.clock ->
  unit ->
  (update_result, update_error) result
(** [update_catalog ?source ~fs ~net ~clock ()] refreshes the cache from the catalog
    endpoint. Existing ETags are sent conditionally. A [304] leaves the cache untouched
    and returns [Not_modified]. Transport, HTTP, and write failures leave the previous
    cache untouched. *)
