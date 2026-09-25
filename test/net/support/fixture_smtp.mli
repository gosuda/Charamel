(** Loopback SMTP fixture, the Lwt port of the Eio harness in [test/pop].

    [t] speaks the same reply vocabulary as the original: greeting, [EHLO] with capability
    lines, [HELO], [AUTH PLAIN], [AUTH LOGIN] challenges, [MAIL FROM]/[RCPT TO], [DATA]
    terminated by a line containing only ["."], any other command, and [QUIT]. The message
    body collected during [DATA] is readable afterwards. *)

type t
(** The type for a running SMTP fixture server. *)

val with_server : (t -> unit Lwt.t) -> unit Lwt.t
(** [with_server f] binds [127.0.0.1:0], serves sessions until [f] finishes, and closes
    the listener afterwards. *)

val port : t -> int
(** [port t] is the TCP port the fixture bound. *)

val body : t -> string option
(** [body t] is the [DATA] payload of the most recent message, lines joined by [CRLF]. *)

val sessions : t -> int
(** [sessions t] is the number of sessions served so far. *)
