(** SMTP delivery for [pop].

    [Smtp] speaks RFC 5321 SMTP with STARTTLS (RFC 3207) or implicit TLS on connect,
    [AUTH PLAIN]/[AUTH LOGIN] (RFC 4954), multiline EHLO replies, [MAIL]/[RCPT]/[DATA]
    with dot-stuffing, and a finite deadline on every operation. The transport is any
    [Eio.Flow.two_way]; the CLI passes a TCP flow or an [Eio_mock.Flow] in tests.

    The error variant carries the server's numeric reply where one exists, so a caller can
    distinguish "server said 550" from "connection failed". *)

type t
(** An established session over an active flow. *)

type reply = { code : int; lines : string list }
(** A server reply: the three-digit [code] and the text of each line, e.g.
    [250-smtp.example.com] then [250 8BITMIME] gives
    [250; ["smtp.example.com"; "8BITMIME"]]. *)

val pp_reply : Format.formatter -> reply -> unit
(** [pp_reply ppf r] renders [r] as its wire form. *)

type response_error =
  [ `Unexpected of reply
    (** A reply arrived whose code was not in the range the step required. *)
  | `Bad_reply of string  (** A line could not be parsed as an SMTP reply. *)
  | `Data_refused of reply  (** The [DATA] command was not accepted with [354]. *)
  | `Tls_refused of reply  (** The server declined [STARTTLS] after advertising it. *)
  | `Auth_refused of reply  (** The server rejected the credentials. *)
  | `Auth_required  (** The server requested credentials but none were supplied. *) ]
(** Errors carrying a server reply. *)

type error =
  [ response_error
  | `Timeout
  | `Closed
  | `Tls of Tls.Engine.failure
  | `Net of Eio.Net.connection_failure
  | `No_recipients
  | `Invalid_address of string
  | `Header_injection of string ]
(** Every failure [connect], [send] and [quit] can produce. [`Timeout] is the session
    deadline. [`Closed] is the peer closing the connection. [`Tls f] is a handshake
    failure with reason [f]. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] renders [e] for CLI and log output. *)

type security =
  | Plain
  | Starttls
  | Tls
      (** The transport security to use: [Plain] is port 25/587 without TLS, [Starttls]
          upgrades after EHLO (RFC 3207), [Tls] is implicit TLS on connect (port 465). *)

val connect :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  host:string ->
  port:int ->
  security:security ->
  ?hostname:string ->
  ?timeout:float ->
  ?tls_config:Tls.Config.client ->
  unit ->
  (t, error) result
(** [connect ~sw ~clock ~net ~host ~port ~security ?hostname ?timeout ?tls_config ()]
    resolves [host] over [net], connects, and on [Tls] wraps the flow in TLS with
    [tls_config] (or a CA-anchored default) before speaking. [hostname] is the EHLO and
    TLS SNI argument, default [host]. [timeout] bounds every exchange, default [30.]
    seconds. The connection, TLS upgrade and initial EHLO all run under the deadline. *)

val send :
  ?helo:string ->
  ?auth:string * string ->
  from:string ->
  recipients:string list ->
  body:string ->
  t ->
  (unit, error) result
(** [send ?helo ?auth ~from ~recipients ~body t] drives one delivery over an established
    {!connect} session: it sends EHLO (falling back to HELO), optional [AUTH PLAIN] or
    [AUTH LOGIN] according to the server's EHLO features, [MAIL FROM], one [RCPT TO] per
    recipient, [DATA] with dot-stuffing, and [QUIT]. [helo] overrides the EHLO argument
    from {!connect}; [auth] is the username/password pair. [recipients] empty is
    [`No_recipients]. The message is normalised to CRLF and terminated with [CRLF.CRLF].
*)

val deliver :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  host:string ->
  port:int ->
  security:security ->
  ?hostname:string ->
  ?timeout:float ->
  ?tls_config:Tls.Config.client ->
  ?helo:string ->
  ?auth:string * string ->
  from:string ->
  recipients:string list ->
  body:string ->
  unit ->
  (unit, error) result
(** [deliver ... ()] is [connect] followed by [send]. It is the one-shot entry point for a
    CLI that sends one message and does not need to reuse a session. The flow is closed
    even when [send] returns an error. *)

val quit : t -> (unit, error) result
(** [quit t] sends [QUIT] and waits for [221], then closes the flow. *)

val close : t -> unit
(** [close t] closes the flow without sending [QUIT]. *)
