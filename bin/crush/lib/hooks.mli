(** Hook process execution for tool and session lifecycle events.

    Hooks receive one bounded JSON request on standard input. Pre-tool hooks authorize a
    call, may replace its decoded input, and fail closed; lifecycle hooks are
    observational. *)

type t
(** The configured hook runner. *)

val create : config:Config.hook list -> cwd:string -> t
(** [create ~config ~cwd] prepares hooks in configuration order. Invalid Perl matchers
    raise [Invalid_argument] with the offending command. *)

type pre_tool_outcome =
  | Allow of Jsont.json
  | Deny of string  (** The result of applying all matching pre-tool hooks. *)

val pre_tool :
  t -> session:string -> tool:string -> input:Jsont.json -> pre_tool_outcome Lwt.t
(** [pre_tool t ~session ~tool ~input] runs matching pre-tool hooks in order. A hook
    answers through its exit status and standard output:

    - exit 0 with blank output allows the call unchanged;
    - exit 0 with a JSON object whose ["decision"] is ["allow"] allows the call, and its
      ["input"] member, when present, replaces the input for the next hook;
    - exit 0 with ["decision"] set to ["deny"] denies the call with the ["reason"] string,
      or ["denied by hook"] without one;
    - exit 2 denies the call with the trimmed standard error, or ["denied by hook"] when
      it is blank.

    Every other outcome denies the call: a hook that cannot be started, exits with any
    other status, prints output that is not such a JSON object, prints standard output of
    more than 1 MiB, or outlives its timeout. These denials carry no process output: the
    reason is ["pre-tool hook failed"], or ["hook <command> timed out"] for a timeout, and
    the detail goes to the [crush.hooks] log source. A denial stops the chain. *)

val post_tool :
  t ->
  session:string ->
  tool:string ->
  input:Jsont.json ->
  output:string ->
  is_error:bool ->
  unit Lwt.t
(** [post_tool] runs matching post-tool hooks. Hook failures are logged and do not change
    the already-produced tool result. *)

val session_start : t -> session:string -> unit Lwt.t
(** [session_start] notifies matching session-start hooks. *)

val stop :
  t -> session:string -> reason:[ `Stop | `Interrupted | `Error of string ] -> unit Lwt.t
(** [stop] notifies matching stop hooks, including a textual error message when [reason]
    is [`Error]. *)

val has : t -> Config.hook_event -> bool
(** [has t event] is [true] when at least one hook is configured for [event]. *)
