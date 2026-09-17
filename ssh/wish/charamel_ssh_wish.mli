(** SSH session middleware and an Eio-backed SSH server.

    The server terminates the SSH protocol with [awa], then exposes one typed session to a
    middleware chain. A session can run an interactive shell or a single exec request, and
    its byte streams are suitable for {!Charamel_tea.Terminal.custom}. *)

type pty = { term : string; rows : int; cols : int }
(** The terminal requested by the client. *)

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

  val remote_addr : t -> Eio.Net.Sockaddr.stream
  (** [remote_addr t] is the peer address. *)

  val resize_events : t -> (int * int) Eio.Stream.t
  (** [resize_events t] receives [(rows, cols)] after every window change. *)

  val stdin : t -> Eio.Flow.source_ty Eio.Resource.t
  (** [stdin t] is the channel input source. It reaches EOF after the client sends SSH
      channel EOF. *)

  val stdout : t -> Eio.Flow.sink_ty Eio.Resource.t
  (** [stdout t] is the channel output sink. *)

  val stderr : t -> Eio.Flow.sink_ty Eio.Resource.t
  (** [stderr t] is the channel extended-data sink. *)

  val exit : t -> int -> unit
  (** [exit t code] requests the SSH exit status, EOF, and close exactly once. The packets
      are sent after the endpoint has returned, so outer middlewares can append final
      output. *)
end

type handler = Session.t -> unit
(** A middleware endpoint. *)

type middleware = handler -> handler
(** A middleware wraps an endpoint and may reject or decorate a session. *)

val tea :
  env:Eio_unix.Stdenv.base -> (Session.t -> ('model, 'msg) Charamel_tea.app) -> middleware
(** [tea ~env make] runs the application returned by [make] over the session's SSH streams
    and current PTY size. Window-change requests are delivered through the custom
    terminal's resize stream. [env] supplies the process capabilities required by the
    public Tea runtime. *)

val active_term : middleware
(** [active_term] rejects sessions without a PTY, writing [Requires an active PTY] and
    exiting with status 1. *)

val access_control : authorized:Awa.Hostkey.pub list -> middleware
(** [access_control ~authorized] permits only sessions authenticated by one of the listed
    public keys. Rejected sessions receive [Access denied] and exit with status 1. *)

val logging : middleware
(** [logging] records one structured connect and disconnect event through [Logs],
    including user, address, command, PTY, and authentication mode. *)

val rate_limit : per_second:float -> burst:int -> middleware
(** [rate_limit ~per_second ~burst] allows at most [burst] new sessions at the configured
    token rate. A rejected session receives a diagnostic and exits with status 1. *)

val elapsed : middleware
(** [elapsed] writes the session duration as [elapsed time: <duration>s] after the wrapped
    handler returns. *)

val serve :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  host_key:Charamel_ssh_keygen.t ->
  addr:Eio.Net.Sockaddr.stream ->
  ?idle_timeout:float ->
  ?max_timeout:float ->
  ?banner:string ->
  ?public_key_auth:(user:string -> Awa.Hostkey.pub -> bool) ->
  ?password_auth:(user:string -> string -> bool) ->
  handler ->
  unit
(** [serve ~sw ~net ~clock ~host_key ~addr handler] listens on [addr] and serves
    authenticated SSH connections until [sw] is released. [idle_timeout] and [max_timeout]
    are disabled by default; when present they are in seconds, and an idle or total
    timeout closes the connection. [banner] is sent during user authentication. Missing
    authentication callbacks reject that method. Only Ed25519 host keys are accepted by
    [awa 0.6.1]. *)
