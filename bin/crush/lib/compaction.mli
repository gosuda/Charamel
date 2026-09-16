(** Session context compaction.

    Compaction retains a recent message tail and asks the small model for a durable
    summary of the older conversation. It writes the summary as a session event before
    returning. *)

val reserve : context_window:int -> int
(** [reserve ~context_window] is the reserved token budget. It is the smaller of 20,000
    tokens and 20 percent of [context_window]. A zero window reserves zero tokens. *)

val needed :
  context_window:int ->
  prompt_tokens:int ->
  completion_tokens:int ->
  disabled:bool ->
  bool
(** [needed ~context_window ~prompt_tokens ~completion_tokens ~disabled] is [true] when
    remaining capacity is at or below [reserve ~context_window] and compaction is not
    disabled. A zero context window never needs compaction. *)

val split : Session.event array -> keep_tokens:int -> int * Session.event list
(** [split events ~keep_tokens] is the index of the last event replaced by a summary and
    the recent event suffix retained verbatim. Message sizes are estimated from their
    UTF-8 bytes divided by four. A result of [-1] means that no event is replaced. *)

val run :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:Eio_unix.Net.t ->
  small:Models.resolved ->
  auth:Charm_fantasy.Provider.auth ->
  Session.t ->
  (string, [ `Provider of string | `Session of Session.error ]) result
(** [run ~sw ~clock ~net ~small ~auth session] summarizes the replaced prefix of [session]
    with [small] and appends the summary. The provider stream is consumed in order and all
    text deltas are retained. *)
