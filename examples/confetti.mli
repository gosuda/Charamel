(** A tiny confetti animation served over SSH. *)

val run : Eio_unix.Stdenv.base -> unit
(** [run env] serves the confetti animation on TCP port 2222. *)
