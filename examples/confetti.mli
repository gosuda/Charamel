(** A tiny confetti animation served over SSH. *)

val run : unit -> unit Lwt.t
(** [run ()] serves the confetti animation on TCP port 2222. *)
