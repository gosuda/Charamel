(** SSH server transport: awa-mirage over [Lwt_unix] sockets.

    Pure-OCaml SSH with no OpenSSL and no foreign stubs: {!module:Flow} adapts an
    [Lwt_unix.file_descr] to {!Mirage_flow.S} and {!module:Ssh} is [Awa_mirage.Make] over
    it. {!val:listen} owns the accept loop; {!val:serve_connection} is the
    single-connection seam for callers — such as an SSH server middleware — that already
    own the socket and its lifecycle.

    Authentication is a user database: a public-key offer must both verify
    cryptographically and appear in that user's key list, and a password must match the
    user's password. The comparison is over SHA-256 digests, so a missing password never
    matches. *)

type user
(** The type for one accepted SSH user. *)

val make_user : string -> ?password:string -> Awa.Hostkey.pub list -> user
(** [make_user name ?password keys] is the user [name], accepted by [password] and/or by
    any key in [keys]; [Invalid_argument] when neither is supplied. *)

val lookup_user : string -> user list -> user option
(** [lookup_user name users] is the user named [name], if any. *)

module Flow : sig
  (** A {!Mirage_flow.S} byte stream over an [Lwt_unix] file descriptor. *)

  type flow
  (** The type for a flow: a socket descriptor and its read buffer. *)

  val create : ?buffer_size:int -> Lwt_unix.file_descr -> flow
  (** [create ?buffer_size fd] wraps [fd] with a read buffer of [buffer_size] bytes
      (default 4096). The descriptor is not set to non-blocking mode here; Lwt owns it. *)

  type error = Unix.error
  (** The type for read errors: the underlying [Unix] error. *)

  val pp_error : error Fmt.t
  (** [pp_error] formats a read error. *)

  type write_error = Mirage_flow.write_error
  (** The type for write errors; every [Unix] failure on the write direction is reported
      as [`Closed]. *)

  val pp_write_error : write_error Fmt.t
  (** [pp_write_error] formats a write error. *)

  val read : flow -> (Cstruct.t Mirage_flow.or_eof, error) result Lwt.t
  (** [read flow] returns at least one byte, freshly copied, [`Eof] at end of input, or
      the [Unix] error that the read failed with. *)

  val write : flow -> Cstruct.t -> (unit, write_error) result Lwt.t
  (** [write flow buffer] writes [buffer] in full, looping over partial writes; the
      promise resolves once the bytes are accepted. *)

  val writev : flow -> Cstruct.t list -> (unit, write_error) result Lwt.t
  (** [writev flow buffers] writes [buffers] in order, stopping at the first failure. *)

  val shutdown : flow -> [ `read | `write | `read_write ] -> unit Lwt.t
  (** [shutdown flow mode] half-closes [flow]; an already-closed descriptor is not an
      error. *)

  val close : flow -> unit Lwt.t
  (** [close flow] releases the descriptor; closing twice is not an error. *)
end

module Ssh : module type of Awa_mirage.Make (Flow)
(** The SSH protocol machinery: {!val:Ssh.spawn_server} runs one server connection over a
    {!module:Flow}, and {!val:Ssh.client_of_flow} is the matching client seam. *)

type request = Ssh.request
(** The type for a request an SSH client made on its channel: a pty allocation or resize,
    an environment variable, an exec/subsystem command, or a shell — the last three
    carrying [ic]/[oc]/[ec] byte-stream closures. *)

type exec_callback = request -> unit Lwt.t
(** The type for the handler invoked for every channel request. *)

type error = Ssh.error
(** The type for SSH connection failures: a protocol message, a flow read error, or a flow
    write error. *)

type write_error = Ssh.write_error
(** The type for SSH write failures. *)

val pp_error : error Fmt.t
(** [pp_error] formats an SSH connection failure. *)

val serve_connection :
  ?stop:Lwt_switch.t ->
  host_key:Awa.Hostkey.priv ->
  users:user list ->
  exec:exec_callback ->
  Flow.flow ->
  (unit, error) result Lwt.t
(** [serve_connection ?stop ~host_key ~users ~exec flow] performs the SSH version
    exchange, key exchange, and user authentication over [flow], then serves channels
    until the client disconnects or [stop] is turned off. Requests are handled by [exec];
    authentication consults [users]. The caller owns [flow] and must close it. *)

val serve_fd :
  ?stop:Lwt_switch.t ->
  host_key:Awa.Hostkey.priv ->
  users:user list ->
  exec:exec_callback ->
  Lwt_unix.file_descr ->
  (unit, error) result Lwt.t
(** [serve_fd] is {!val:serve_connection} over [Flow.create fd], closing [fd] when the
    connection ends. *)

type server = { port : int; stop : unit -> unit Lwt.t }
(** The type for a running SSH server: the [port] it bound (the requested port, or the one
    assigned when [0] was asked for) and [stop], which ends the accept loop, cancels live
    connections, and releases their descriptors. *)

val listen :
  ?backlog:int ->
  ?address:Unix.inet_addr ->
  port:int ->
  host_key:Awa.Hostkey.priv ->
  users:user list ->
  exec:exec_callback ->
  unit ->
  server Lwt.t
(** [listen ?backlog ?address ~port ~host_key ~users ~exec ()] binds [address:port]
    ([backlog] defaults to 16, [address] to loopback) and serves every accepted connection
    with {!val:serve_fd} in the background. *)
