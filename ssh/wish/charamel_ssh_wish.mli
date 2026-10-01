(** SSH session middleware and an SSH server.

    The server terminates the SSH protocol with [awa] over {!Charamel_net.Ssh_server},
    then exposes one typed session to a middleware chain. A session can run an interactive
    shell or a single exec request, and its byte streams are suitable for
    {!Charamel_tea.Terminal.custom}. *)

type pty = { term : string; rows : int; cols : int }
(** The terminal requested by the client. *)

type address = [ `Tcp of string * int | `Unix of string ]
(** The type for a listen or peer address. [`Tcp] names the host as its textual address —
    an IPv6 host contains a colon and binds to an [inet6] socket, anything else binds to
    [inet] — and the port. [`Unix] names a filesystem socket path. *)

module Session : sig
  type nonrec pty = pty
  (** The terminal requested by the client. *)

  type t
  (** A server-side authenticated SSH session. *)

  val user : t -> string
  (** [user t] is the authenticated login name. *)

  val public_key : t -> Awa.Hostkey.pub option
  (** [public_key t] is the key used for authentication, when public-key authentication
      was used. *)

  val command : t -> string list
  (** [command t] is the shell-word command requested by the client. An interactive shell
      has no command words. *)

  val pty : t -> pty option
  (** [pty t] is the latest terminal request, or [None] when no PTY was requested. *)

  val env : t -> (string * string) list
  (** [env t] is the environment requested by the client. *)

  val remote_addr : t -> address
  (** [remote_addr t] is the peer address. *)

  val resize_events : t -> (int * int) Lwt_stream.t
  (** [resize_events t] receives [(rows, cols)] after every window change. It is a
      single-consumer stream: exactly one reader, normally {!val:Charamel_ssh_wish.tea},
      may take from it. *)

  val stdin : t -> Lwt_io.input_channel
  (** [stdin t] is the channel input source. It reaches EOF after the client sends SSH
      channel EOF. *)

  val stdout : t -> Lwt_io.output_channel
  (** [stdout t] is the channel output sink. *)

  val stderr : t -> Lwt_io.output_channel
  (** [stderr t] is the channel extended-data sink. *)

  val exit : t -> int -> unit Lwt.t
  (** [exit t code] requests the SSH exit status, EOF, and close exactly once. The packets
      are sent after the endpoint has returned, so outer middlewares can append final
      output; the returned promise resolves once they have reached the wire. *)

  val print : t -> string -> unit Lwt.t
  (** [print t text] writes [text] to the session output and flushes it. *)

  val print_line : t -> string -> unit Lwt.t
  (** [print_line t text] writes [text] and a newline to the session output. *)

  val write_string : t -> string -> int Lwt.t
  (** [write_string t text] writes [text] to the session output and is the number of bytes
      written, which is the length of [text]. *)

  val error : t -> string -> unit Lwt.t
  (** [error t text] writes [text] to the session error stream and flushes it. *)

  val error_line : t -> string -> unit Lwt.t
  (** [error_line t text] writes [text] and a newline to the session error stream. *)

  val fatal : t -> string -> unit Lwt.t
  (** [fatal t text] writes [text] to the session error stream and requests exit status 1.
      It does not close the channel, because {!val:exit} already performs that. *)
end

type handler = Session.t -> unit Lwt.t
(** A middleware endpoint. *)

type middleware = handler -> handler
(** A middleware wraps an endpoint and may reject or decorate a session. *)

val tea :
  env:Charamel_cli.Env.t -> (Session.t -> ('model, 'msg) Charamel_tea.app) -> middleware
(** [tea ~env make] runs the application returned by [make] over the session's SSH streams
    and current PTY size. Window-change requests are delivered through the custom
    terminal's resize stream. [env] supplies the process capabilities required by the
    public Tea runtime. When the session has an allocated pseudo-terminal, as
    {!val:allocate_pty} gives it, the colour profile comes from the session environment
    instead of being negotiated and {!val:Charamel_tea.Cmd.exec} runs its child on that
    pseudo-terminal: the command sees the client's terminal, its output reaches the
    client, and its exit code is the one delivered to the program's [on_exit] handler.
    Without an allocated pseudo-terminal [Cmd.exec] runs its child on this process's own
    descriptors, so an application that must run a program inside the session can call
    {!val:command} instead. A platform that cannot allocate a pseudo-terminal — Windows
    reports so through {!val:allocate_pty} — therefore has no in-session [Cmd.exec]. *)

val tea_with_stream :
  env:Charamel_cli.Env.t ->
  (Session.t -> ('model, 'msg) Charamel_tea.app * 'msg Lwt_stream.t) ->
  middleware
(** [tea_with_stream ~env make] is {!val:tea} for an application that also reads an
    external event source. [make] returns the application and its stream, and the
    middleware subscribes the application to the stream through
    {!val:Charamel_tea.Sub.stream}, so messages produced by another fiber or program reach
    [update] like any other input. A stream that ends simply stops delivering. *)

val active_term : middleware
(** [active_term] rejects sessions without a PTY, writing [Requires an active PTY] and
    exiting with status 1. *)

val allocate_pty : middleware
(** [allocate_pty] gives each session a pseudo-terminal of its own, sized from the
    client's terminal request and resized by every later window change. Programs that
    {!val:command} runs, and the children {!val:Charamel_tea.Cmd.exec} runs inside a
    {!val:tea} handler, attach to it, so [isatty] is true for them, while the session's
    own streams stay on the SSH channel. A platform that cannot allocate a pseudo-terminal
    — Windows reports so here — gets the reason on the error stream and exits with status
    1. When the wrapped handler returns, the last child group is signalled to stop and the
    terminal is released. *)

val emulated_pty : Session.t -> bool
(** [emulated_pty session] is [true] when the session has no allocated pseudo-terminal.
    That is the case a Tea program serves by redrawing over the channel itself. *)

val command :
  ?dir:string ->
  ?env:string list ->
  Session.t ->
  string ->
  string list ->
  (int, [> `Failed of string ]) result Lwt.t
(** [command ?dir ?env session program args] runs [program] with [args] on the session's
    pseudo-terminal and is its exit status. The child's environment is this process's
    environment with the client-requested variables and [env] applied last. Session input
    reaches the child's terminal, the child's output reaches the session, and the input
    pump stops when the child exits. [dir] sets the working directory. Without
    {!val:allocate_pty} the call allocates a terminal for its own duration and releases it
    afterwards. [`Failed] carries the reason: no pseudo-terminal on this platform, a
    program that could not be started, or a child that could not be waited for. *)

val subsystem : (string * handler) list -> middleware
(** [subsystem handlers] dispatches a session that requested a subsystem. A listed name
    runs that handler; an unlisted name is reported as [unknown subsystem: <name>] on the
    error stream and exits with status 1. Every other session runs the wrapped handler. *)

val password_or_key_auth : authorized:Awa.Hostkey.pub list -> middleware
(** [password_or_key_auth ~authorized] permits only sessions authenticated by one of the
    listed public keys. Rejected sessions receive [Access denied] and exit with status 1.
*)

val authorized_keys_file : path:string -> middleware
(** [authorized_keys_file ~path] admits only sessions whose authentication key is listed
    in the [authorized_keys] document at [path], which is read afresh for every session.
    The file is parsed by {!val:Charamel_ssh_keygen.parse_authorized_keys} and matched on
    the public key blob, so both [ssh-ed25519] and [ssh-rsa] entries work. A session that
    is not listed, and any session seen while the file cannot be read, receives
    [Access denied] and exits with status 1. Certificates are not matched: [awa 0.6.1]
    exposes no certificate API. *)

val allow_commands : string list -> middleware
(** [allow_commands allowed] runs the wrapped handler for an interactive session and for a
    session whose first command word is listed. Any other command is reported as
    [Command is not allowed: <program>] on the error stream and exits with status 1. *)

val comment : string -> middleware
(** [comment text] runs the wrapped handler and then writes [text] and a newline to the
    session output. *)

val recover : middleware
(** [recover] turns an exception raised by the wrapped chain into a logged [panic] with
    its backtrace and an exit status of 1, so one broken session cannot end the server.
    Only the task that runs the chain is guarded. *)

val logging : middleware
(** [logging] records one structured connect and disconnect event through [Logs],
    including user, address, command, PTY, and authentication mode. *)

type rate_limiter = Session.t -> bool
(** A rate limiter decides whether one session may proceed. *)

val token_bucket :
  ?max_entries:int ->
  per_second:float ->
  burst:int ->
  unit ->
  (string -> bool) * (unit -> int)
(** [token_bucket ?max_entries ~per_second ~burst ()] is a token bucket keyed by an
    arbitrary string, with the count of keys it retains. A key starts with [burst] tokens
    and regains [per_second] tokens each second up to that ceiling; [allow key] takes one
    token and is [true] when it found one. The bucket keeps at most [max_entries] keys,
    1024 by default, and forgets the least recently seen key when a new key would overflow
    it. A forgotten key starts full again. *)

val rate_limit_custom : rate_limiter -> middleware
(** [rate_limit_custom limiter] runs the wrapped handler for a session the limiter allows.
    A rejected session receives [rate limit exceeded, please try again later] and exits
    with status 1. *)

val rate_limit : ?max_entries:int -> per_second:float -> burst:int -> middleware
(** [rate_limit ?max_entries ~per_second ~burst] limits sessions per remote address with a
    {!val:token_bucket}. *)

val elapsed_with_format : string -> middleware
(** [elapsed_with_format template] writes the session duration through [template], which
    must hold exactly one floating-point placeholder such as [%.3f], after the wrapped
    handler returns. Put it last in the chain: its line is the session's final output.
    @raise Invalid_argument when [template] does not hold one float placeholder. *)

val elapsed : middleware
(** [elapsed] is {!val:elapsed_with_format} with [elapsed time: %.3fs]. *)

val serve :
  ?stop:Lwt_switch.t ->
  host_key:Charamel_ssh_keygen.t ->
  addr:address ->
  ?idle_timeout:float ->
  ?max_timeout:float ->
  ?banner:string ->
  ?banner_handler:(Session.t -> string option) ->
  ?public_key_auth:(user:string -> Awa.Hostkey.pub -> bool) ->
  ?password_auth:(user:string -> string -> bool) ->
  handler ->
  unit ->
  unit Lwt.t
(** [serve ?stop ~host_key ~addr handler ()] listens on [addr] and serves authenticated
    SSH connections until the promise is cancelled or [stop] is turned off, which also
    ends the live connections. [idle_timeout] and [max_timeout] are disabled by default;
    when present they are in seconds, and an idle or total timeout closes the connection.
    Missing authentication callbacks reject that method. [host_key] must be an Ed25519
    pair, which is all [awa 0.6.1] accepts as a host key, and the server presents exactly
    that pair: the fingerprint and the [.pub] line that {!Charamel_ssh_keygen.write}
    produced are what a client verifies. The server identification string is fixed by
    [awa] and cannot be configured.

    The banner is sent once, during the first authentication attempt. [banner_handler]
    computes it per session and is consulted at that attempt, before the session is
    authenticated, so {!val:Session.user} is still empty; [None] sends no banner. Without
    a handler, [banner] is the text sent. *)
