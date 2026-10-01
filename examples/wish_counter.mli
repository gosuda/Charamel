(** A small counter served over SSH. *)

val run : unit -> unit Lwt.t
(** [run ()] serves the counter on TCP port 2222. *)
