(** Hook process execution for tool and session lifecycle events.

    Hooks receive one bounded JSON request on standard input. Pre-tool hooks may deny a
    call or replace its decoded input; lifecycle hooks are observational. *)

type t
(** The configured hook runner. *)

val create :
  config:Config.hook list ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  cwd:string ->
  t
(** [create ~config ~proc_mgr ~clock ~cwd] prepares hooks in configuration order. Invalid
    Perl matchers raise [Invalid_argument] with the offending command. *)

type pre_tool_outcome =
  | Allow of Jsont.json
  | Deny of string  (** The result of applying all matching pre-tool hooks. *)

val pre_tool : t -> session:string -> tool:string -> input:Jsont.json -> pre_tool_outcome
(** [pre_tool t ~session ~tool ~input] runs matching pre-tool hooks in order. An allow
    response may replace the input for the next hook. A denial stops the chain. Process
    failures are logged and treated as a pass; timeout and exit 2 are explicit denials. *)

val post_tool :
  t ->
  session:string ->
  tool:string ->
  input:Jsont.json ->
  output:string ->
  is_error:bool ->
  unit
(** [post_tool] runs matching post-tool hooks. Hook failures are logged and do not change
    the already-produced tool result. *)

val session_start : t -> session:string -> unit
(** [session_start] notifies matching session-start hooks. *)

val stop :
  t -> session:string -> reason:[ `Stop | `Interrupted | `Error of string ] -> unit
(** [stop] notifies matching stop hooks, including a textual error message when [reason]
    is [`Error]. *)

val has : t -> Config.hook_event -> bool
(** [has t event] is [true] when at least one hook is configured for [event]. *)
