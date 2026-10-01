(** Provider credentials and the shared persistent authentication resource.

    [Auth] owns one live resource for one credential path. Every mutating operation
    reloads the file while holding the resource mutex and the stable lock file, then
    publishes its result into that resource. *)

type credential =
  | Api_key of string
  | Oauth of Charamel_fantasy.Oauth.Credential.t
  | Disabled of { reason : string; at_ms : int }
      (** The type for a stored provider credential. *)

type error = [ `Io of string * string | `Parse of string * string ]
(** The type for persistent authentication failures. *)

type refresh_error =
  [ error | `Disabled of string | `No_credential | `Refresh of Charamel_fantasy.Error.t ]
(** The type for authentication and OAuth-refresh failures. *)

type login_error = [ error | `Oauth of string | `Timeout | `Aborted ]
(** The type for login failures. *)

type t
(** The type for a shared authentication resource. *)

val pp_error : error Fmt.t
(** [pp_error ppf error] formats an authentication failure without secrets. *)

val pp_refresh_error : refresh_error Fmt.t
(** [pp_refresh_error ppf error] formats an authentication or OAuth-refresh failure
    without secrets. *)

val path : unit -> string
(** [path ()] is the XDG credential path used by the CLI boundary. *)

val jsont : (string * credential) list Jsont.t
(** [jsont] is the object-shaped credential file codec. *)

val create :
  path:string -> clock:Charamel_os.Time.clock -> unit -> (t, error) result Lwt.t
(** [create ~path ~clock ()] captures [path] and [clock], loads the credential file, and
    returns a fresh resource. A missing file means no stored entries. *)

val find : t -> provider:string -> credential option
(** [find resource ~provider] is the cached credential for [provider]. This is a
    diagnostic view and does not perform a disk reload. *)

val providers : t -> string list
(** [providers resource] lists cached provider ids in stable file order. *)

val set : t -> provider:string -> credential -> (unit, error) result Lwt.t
(** [set resource ~provider credential] persists one provider replacement and publishes it
    into [resource]. *)

val remove : t -> provider:string -> (unit, error) result Lwt.t
(** [remove resource ~provider] persistently removes one provider and publishes the result
    into [resource]. *)

val to_fantasy : credential -> Charamel_fantasy.Provider.auth option
(** [to_fantasy credential] converts usable credentials and returns [None] for disabled
    credentials. *)

val resolve :
  t ->
  config:Config.t ->
  env:(string -> string option) ->
  provider:string ->
  (credential option, error) result Lwt.t
(** [resolve resource ~config ~env ~provider] reloads the resource and applies stored,
    configured, and environment API-key precedence. Fallback keys are not persisted. A
    persistence-faulted resource returns its fault. *)

val ensure_fresh :
  config:Config.t ->
  env:(string -> string option) ->
  t ->
  provider:string ->
  (Charamel_fantasy.Provider.auth, refresh_error) result Lwt.t
(** [ensure_fresh ~config ~env resource ~provider] reloads the resource, returns a
    configured or environment API key without persisting it, and refreshes an expiring
    OAuth credential under the resource transaction. *)

val refresh :
  config:Config.t ->
  env:(string -> string option) ->
  t ->
  provider:string ->
  rejected:Charamel_fantasy.Provider.auth ->
  (Charamel_fantasy.Provider.auth, refresh_error) result Lwt.t
(** [refresh ~config ~env resource ~provider ~rejected] performs a conditional OAuth
    recovery. It rotates only when [rejected] is still the current stored credential; a
    replacement is returned through normal freshness handling. *)

val disable :
  t ->
  provider:string ->
  rejected:Charamel_fantasy.Provider.auth ->
  reason:string ->
  now_ms:int ->
  (unit, error) result Lwt.t
(** [disable resource ~provider ~rejected ~reason ~now_ms] disables only the credential
    equal to [rejected], so a newer login or rotation is never disabled. *)

module Login : sig
  val port : int
  (** [port] is the loopback callback port. *)

  val redirect_uri : string
  (** [redirect_uri] is the callback URI used for OAuth. *)

  val anthropic :
    open_browser:(string -> unit) ->
    prompt_paste:(unit -> string option Lwt.t) ->
    t ->
    (unit, login_error) result Lwt.t
  (** [anthropic ~open_browser ~prompt_paste resource] completes an Anthropic browser or
      paste login and mutates [resource] through its persistent Anthropic entry.
      [open_browser] is asked to show the authorization URL and is not awaited;
      [prompt_paste] may be cancelled when the loopback callback wins the race.
      Interactive waiting does not hold the store lock. *)
end
