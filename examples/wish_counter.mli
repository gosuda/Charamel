(** A small counter served over SSH. *)

val run : Eio_unix.Stdenv.base -> unit
(** [run env] serves the counter on TCP port 2222. *)
