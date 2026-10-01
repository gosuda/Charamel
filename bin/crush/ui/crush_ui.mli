(** Crush's interactive terminal application.

    The UI owns the Tea model and keeps the agent event bridge alive for the duration of a
    run. Core services are supplied by [backend]; the UI never fabricates model, session,
    token, or provider state. *)

type session = { id : string; title : string; model : string; created_ms : int }
(** A session row shown by the session picker and sidebar. *)

type model = {
  id : string;
  provider : string;
  context_window : int;
  max_tokens : int;
  can_reason : bool;
  supports_attachments : bool;
}
(** A model row shown by the model picker. *)

type history_item = { role : string; text : string }
(** A persisted conversation row supplied by the session store. *)

module Bridge : sig
  type ask_request
  type permission_request
  type t

  val create : ?capacity:int -> unit -> t
  (** [create ?capacity ()] creates a bounded, lossless bridge. The default capacity is
      256. *)

  val push : t -> Crush_core.Agent.event -> unit Lwt.t
  (** [push bridge event] blocks while the bounded event queue is full. Once [close] has
      run it returns without dropping a producer's cancellation. *)

  val ask :
    t ->
    Crush_core.Tool.question list ->
    (Crush_core.Tool.answer list, [ `Aborted | `Not_interactive ]) result Lwt.t
  (** [ask bridge questions] serializes an agent question through the UI. *)

  val ask_permission :
    t -> Crush_core.Permission.request -> Crush_core.Permission.decision Lwt.t
  (** [ask_permission bridge request] serializes one decoded write request through the
      permission dialog and blocks until the policy decision. *)

  val take_event : t -> Crush_core.Agent.event option Lwt.t
  val take_question : t -> ask_request option Lwt.t

  val take_permission : t -> permission_request option Lwt.t
  (** [take_permission t] takes the next permission request for [t], blocking until one
      arrives. It returns [None] only after [close] and once the queue is empty. *)

  val questions : ask_request -> Crush_core.Tool.question list

  val answer :
    t ->
    ask_request ->
    (Crush_core.Tool.answer list, [ `Aborted | `Not_interactive ]) result ->
    unit

  val permission : permission_request -> Crush_core.Permission.request

  val answer_permission :
    t -> permission_request -> Crush_core.Permission.decision -> unit

  val close : t -> unit
  (** [close] wakes every blocked producer, consumer, and dialog waiter. *)
end

type backend = {
  agent : Crush_core.Agent.t ref;
  events : Bridge.t;
  clock : Charamel_os.Time.clock;
  form_env : Charamel_huh.Form.Env.t;
  project : string;
  session_id : unit -> string;
  new_session : unit -> (Crush_core.Agent.t, string) result;
  sessions : unit -> session list;
  resume_session : string -> (Crush_core.Agent.t, string) result;
  history : unit -> history_item list;
  models : unit -> model list;
  select_model : string -> (unit, string) result;
  login : string -> string -> (unit, string) result;
  logout : string -> (unit, string) result;
  load_attachment : string -> (string * string * string option, string) result;
      (** [load_attachment path] returns MIME, raw bytes, and an optional display name.
          The UI base64-encodes those bytes exactly once before invoking [Agent.prompt];
          callbacks must not pre-encode them. *)
  yolo : unit -> bool;
  approve_session : unit -> (unit, string) result;
  set_plan_mode : bool -> (unit, string) result;
  plan_mode : unit -> bool;
  lsp_status : unit -> string;
  mcp_status : unit -> string;
  dark : unit -> bool;
  quit : unit -> unit;
}
(** Capabilities and callbacks owned by the CLI/core. The UI invokes these callbacks for
    every real operation; no callback is replaced with a fake label or a local-only state
    transition. *)

type ui_model
(** The mutable UI model used by the Tea application. *)

type ui_msg
(** Internal and external Tea messages. *)

val app : backend -> (ui_model, ui_msg) Charamel_tea.app
(** [app backend] is the Tea application used by both production and scripted tests. *)

val run : backend -> (ui_model, Charamel_tea.error) result Lwt.t
(** [run backend] starts the chat application on the local terminal, restoring it and
    closing the bridge on every exit path. *)

val run_with :
  backend ->
  events:
    [ `Key of Charamel_tea.Key.t
    | `Text of string
    | `Resize of int * int
    | `Msg of ui_msg
    | `Wait of float ]
    list ->
  size:int * int ->
  ui_model * string
(** [run_with] drives the same application against [Charamel_tea.Test] and is the
    deterministic backend bridge surface used by UI tests. The bridge remains open for the
    scripted run and is closed when it returns. *)
