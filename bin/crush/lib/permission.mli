(** Crush permission policy.

    Permission policies apply one ordered decision procedure to every decoded tool
    request. *)

type request = {
  session : string;
  tool : string;
  action : string;
  path : string;
  description : string;
  read_only : bool;
}
(** The type for decoded tool requests. A request path is absolute when it names a file
    target and is empty when no target is known. [read_only] is a trusted classification
    supplied by the tool after decoding its input. *)

type decision =
  | Allow_once
  | Allow_session
  | Deny  (** The type for answers returned by an interactive asker. *)

type outcome =
  | Allowed
  | Denied of string  (** The type for the result of policy resolution. *)

type asker = request -> decision
(** The type for a serialized interactive permission question. *)

type hook = request -> [ `Allow | `Deny of string | `Pass ]
(** The type for a policy hook. *)

type t
(** The type for a mutable permission policy. *)

val create :
  config:Config.permissions ->
  yolo:bool ->
  ?asker:asker ->
  ?hook:hook ->
  ?on_decision:(request -> outcome -> unit) ->
  cwd:string ->
  plans_dir:string ->
  unit ->
  t
(** [create ~config ~yolo ~cwd ~plans_dir ()] is a permission policy. [asker] defaults to
    an asker that denies the request. [hook] defaults to a hook that passes the request.
    [on_decision] defaults to a callback that does nothing. The callback receives each
    resolved request and outcome once, after the policy and asker locks are released.
    [cwd] is the project boundary used for local read-only approval. [plans_dir] is the
    only directory in which a known write or edit target can pass the plan ceiling. *)

val matches : entry:string -> request -> bool
(** [matches ~entry request] is [true] when [entry] names [request.tool] or names that
    tool and exactly [request.action]. A tool-only entry matches every action for that
    tool. *)

val canonical : string -> string
(** [canonical path] resolves dot and dot-dot components lexically. A leading slash is
    preserved and dot-dot above the root is dropped. The empty path stays empty. *)

val within : root:string -> string -> bool
(** [within ~root path] is [true] when [path] and [root] are both absolute and every
    component of [root] is a leading component of [path]. Component boundaries are
    respected. *)

val plan_mode : t -> bool
(** [plan_mode t] is [true] when [t] applies its plan ceiling. *)

val set_plan_mode : t -> bool -> unit
(** [set_plan_mode t enabled] changes whether [t] applies its plan ceiling. *)

val plans_dir : t -> string
(** [plans_dir t] is the canonical plans directory of [t]. *)

val grant_session : t -> request -> unit
(** [grant_session t request] records an exact session grant for [request]. The session,
    tool, action, and canonical path all participate in the grant key. *)

val auto_approve_session : t -> session:string -> unit
(** [auto_approve_session t ~session] records a session-wide approval. A request from
    [session] can use this approval at the session-grant stage unless an earlier rule
    denies it. *)

val resolve : t -> request -> outcome
(** [resolve t request] applies the plan ceiling, yolo override, configured deny and allow
    entries, exact grants, hooks, local read-only approval, and the asker in that order.
    The asker is serialized separately from policy state and hook callbacks. *)

val pp_request : request Fmt.t
(** [pp_request] formats a permission request. *)
