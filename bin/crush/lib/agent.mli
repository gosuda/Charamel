(** The Crush model turn runner.

    The runner owns one session conversation, streams provider output as ordered events,
    executes tool calls, and persists every observable message, result, permission
    decision, and usage record. A prompt is accepted only while the runner is idle. *)

type event =
  | Text_delta of string
  | Reasoning_delta of string
  | Tool_started of { id : string; name : string; input : Jsont.json }
  | Tool_finished of {
      id : string;
      name : string;
      output : Tool.output;
      elapsed_ms : int;
    }
  | Permission_asked of Permission.request
  | Permission_resolved of Permission.request * Permission.outcome
  | Usage of {
      usage : Charm_fantasy.Usage.t;
      cost_usd : float;
      total_cost_usd : float;
      context_tokens : int;
    }
  | Compacted of { summary_chars : int }
  | Title of string
  | Advisor_note of { severity : string; guidance : string }
  | Turn_done of finish
  | Failed of error

and finish =
  [ `Stop
  | `Length
  | `Content_filter
  | `Interrupted
  | `Loop_detected
  | `Budget
  | `Halted of string ]

and error =
  [ `Busy
  | `Provider of string
  | `Session of Session.error
  | `Models of Models.error
  | `Auth of string
  | `Tool of string ]

val pp_error : error Fmt.t
(** [pp_error ppf error] formats an agent failure for a user-facing diagnostic. *)

type deps = {
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  net : Eio_unix.Net.t;
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  random : int -> string;
  env : string -> string option;
  cwd : string;
  config : Config.t;
  auth : Auth.t;
  store : Session.store;
  permission : Permission.t;
  hooks : Hooks.t;
  lsp : Lsp.t option;
  mcp : Mcp.t;
  skills : Skills.t;
  rules : Rules.t;
  log_path : string;
  interactive : bool;
  ask :
    (Tool.question list -> (Tool.answer list, [ `Aborted | `Not_interactive ]) result)
    option;
  events : event -> unit;
}
(** The capabilities and policy captured by an agent. The event sink preserves event order
    and may apply bounded backpressure. *)

type t
(** The type for one session agent. *)

val create :
  deps ->
  session:Session.t ->
  large:Models.resolved ->
  small:Models.resolved ->
  (t, error) result
(** [create deps ~session ~large ~small] is an idle agent attached to [session] with the
    selected large and small models. *)

val session : t -> Session.t
(** [session t] is the session owned by [t]. *)

val large : t -> Models.resolved
(** [large t] is the model used for normal turns. *)

val set_models : t -> large:Models.resolved -> small:Models.resolved -> unit
(** [set_models t ~large ~small] replaces the models used by subsequent turns. *)

val busy : t -> bool
(** [busy t] is [true] while [t] is executing a prompt. *)

val cancel : t -> unit
(** [cancel t] interrupts the current turn and causes it to finish as [`Interrupted]. It
    does not cancel an outer switch. *)

val prompt :
  t -> ?attachments:(string * string * string) list -> string -> (finish, error) result
(** [prompt t ?attachments text] sends [text] to the provider and executes its tool calls.
    Attachment data is already wire-encoded and is passed directly to the provider.
    Attachments are MIME, data, and optional-name triples. A prompt submitted while
    another prompt is active returns [`Busy]. *)

val compact : t -> (unit, error) result
(** [compact t] compacts the current session through the small model. *)

val set_plan_mode : t -> bool -> unit
(** [set_plan_mode t enabled] changes the permission policy and records the mode change in
    the session. *)

val stats : t -> Charm_fantasy.Usage.t * float * int
(** [stats t] is total usage, total cost in US dollars, and the latest context token
    count. *)
