(** Local SMTP fixture for [pop] integration tests. *)

type t
(** The type for a running fixture. *)

val start : sw:Eio.Switch.t -> net:_ Eio.Net.t -> unit -> t
(** [start ~sw ~net ()] starts a plain SMTP server on the loopback interface. The fixture
    accepts one message and records its DATA payload. *)

val port : t -> int
(** [port fixture] is the loopback port selected by the operating system. *)

val body : t -> string option
(** [body fixture] is the most recently received DATA payload, without the final SMTP
    terminator. *)
