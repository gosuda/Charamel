(** Persistent Crush sessions.

    [Session] stores one newline-delimited JSON record per header or event. A session is
    scoped to a project hash, can be replayed after a process interruption, and keeps an
    atomic index for discovery. *)

type model_ref = { provider : string; model : string }
(** A provider identifier and model identifier recorded in a session header. *)

type header = {
  id : string;
  title : string;
  parent : string option;
  created_ms : int;
  cwd : string;
  model : model_ref;
}
(** The first record of every session file. [id] is also the file stem. *)

type tool_output = [ `Text of string | `Error of string | `Media of string * string ]
(** A tool result retained in the session event log. *)

type decision =
  | Allow_once
  | Allow_session
  | Deny  (** A recorded permission decision. *)

type event =
  | Message of { ms : int; message : Charm_fantasy.Message.t }
  | Tool_call of { ms : int; id : string; name : string; input : Jsont.json }
  | Tool_result of {
      ms : int;
      id : string;
      name : string;
      output : tool_output;
      elapsed_ms : int;
      artifact : string option;
    }
  | Usage of {
      ms : int;
      usage : Charm_fantasy.Usage.t;
      cost_usd : float;
      model : model_ref;
    }
  | Summary of { ms : int; text : string; through : int }
  | Permission of {
      ms : int;
      tool : string;
      action : string;
      path : string;
      decision : decision;
    }
  | Note of { ms : int; text : string }
      (** A durable session event. The event order is the provider-visible order. *)

val header_jsont : header Jsont.t
(** [header_jsont] decodes and encodes a session header. *)

val event_jsont : event Jsont.t
(** [event_jsont] decodes and encodes every event variant. The discriminator member [t] is
    emitted first. *)

val message_jsont : Charm_fantasy.Message.t Jsont.t
(** [message_jsont] is the canonical message codec used by [Message] events. *)

val part_jsont : Charm_fantasy.Message.part Jsont.t
(** [part_jsont] is the canonical message-part codec. *)

val tool_output_jsont : tool_output Jsont.t
(** [tool_output_jsont] is the canonical tool-output codec. *)

type index_entry = {
  id : string;
  title : string;
  parent : string option;
  created_ms : int;
  updated_ms : int;
  message_count : int;
  usage : Charm_fantasy.Usage.t;
  cost_usd : float;
}
(** One row in [sessions.json]. *)

val index_jsont : index_entry list Jsont.t
(** [index_jsont] is the JSON array codec for index rows. The on-disk wrapper stores the
    array in a [sessions] member. *)

type store
(** The project-scoped session store. *)

val store : fs:Eio.Fs.dir_ty Eio.Path.t -> cwd:string -> store
(** [store ~fs ~cwd] points at the Crush data directory for the SHA-256 project key
    derived from [cwd]. It does not create directories. *)

val root : store -> string
(** [root store] is the project-scoped session directory. *)

type t
(** An opened session and its serialized event state. *)

type error =
  [ `Io of string * string
  | `Session_corrupt of string * int
  | `Not_found of string
  | `Index of string ]
(** A recoverable session-store failure. [Session_corrupt] carries a path and the first
    bad line number. *)

val pp_error : error Fmt.t
(** [pp_error] formats a session-store error. *)

val create :
  store ->
  clock:_ Eio.Time.clock ->
  random:(int -> string) ->
  ?parent:string ->
  ?title:string ->
  cwd:string ->
  model:model_ref ->
  unit ->
  (t, error) result
(** [create store ~clock ~random ?parent ?title ~cwd ~model ()] allocates a ULID, writes
    its header, and adds an index row. [title] defaults to the empty string. *)

val open_ : store -> id:string -> (t, error) result
(** [open_ store ~id] replays a session. A malformed final event line is treated as a
    killed-process partial append: only that line is removed under cancellation
    protection. A malformed non-final line, a missing header, or a header/file id mismatch
    is [Session_corrupt]. *)

val last : store -> (t, error) result
(** [last store] opens the newest indexed session, or returns [`Not_found ""] when the
    index has no rows. *)

val list : store -> (index_entry list, error) result
(** [list store] reads [sessions.json] and returns rows newest first. *)

val id : t -> string
(** [id session] is the session ULID. *)

val header : t -> header
(** [header session] is the decoded session header. *)

val title : t -> string
(** [title session] is the current in-memory title. *)

val events : t -> event array
(** [events session] is a copy of the replayed event array. *)

val path : t -> string
(** [path session] is the absolute JSONL path. *)

val append : t -> clock:_ Eio.Time.clock -> event -> (unit, error) result
(** [append session ~clock event] serializes and appends one newline-terminated event
    while holding the session mutex. The in-memory events and atomic index row change only
    after the complete write succeeds. *)

val set_title : t -> title:string -> (unit, error) result
(** [set_title session ~title] updates the indexed title without adding an event. *)

val messages : t -> Charm_fantasy.Message.t list
(** [messages session] replays model-visible messages. The latest summary is represented
    by a synthetic user summary and assistant acknowledgement; observability-only tool
    events are not duplicated. *)

val usage_total : t -> Charm_fantasy.Usage.t * float
(** [usage_total session] sums all usage events and their dollar costs. *)

val delete : store -> id:string -> (unit, error) result
(** [delete store ~id] removes the session file and its artifact directory and atomically
    removes its index row. *)

val artifacts_dir : store -> id:string -> string
(** [artifacts_dir store ~id] is the project-scoped artifact directory for [id]. *)
