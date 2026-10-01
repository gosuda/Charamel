(** Bounded background process jobs.

    Jobs run shell commands under one switch, capture merged standard output, and retain a
    finite completed-job history. *)

type t
(** The type for a job registry. *)

val create : sw:Lwt_switch.t -> artifacts:Artifact.t -> t
(** [create ~sw ~artifacts] is an empty registry attached to [sw]. Starting a job
    registers a [sw] hook that stops its process group, so no job outlives the switch. *)

val start :
  t ->
  cwd:string ->
  command:string ->
  env:(string * string) list ->
  timeout_s:int ->
  string
(** [start t ~cwd ~command ~env ~timeout_s] starts [command] through the POSIX shell in
    [cwd] with [env] overrides. [timeout_s] is the maximum runtime in seconds and must be
    positive. It returns a fresh identifier of the form [job-N] without waiting for the
    child. Standard output and standard error are captured together with a ten-megabyte
    bound. *)

type status =
  | Running
  | Exited of int
  | Killed  (** The type for a job lifecycle state. *)

val output :
  t -> id:string -> wait:bool -> (string * status, [ `Not_found of string ]) result Lwt.t
(** [output t ~id ~wait] is the captured output and current status for [id]. When [wait]
    is [true], it waits for completion for at most ten minutes. A timed-out wait returns
    the output collected so far with [Running]. *)

val kill : t -> id:string -> (unit, [ `Not_found of string ]) result Lwt.t
(** [kill t ~id] requests graceful termination of [id], then schedules a forced
    termination after two seconds if the process is still running. *)

val kill_all : t -> unit Lwt.t
(** [kill_all t] requests termination of every running job and waits through the bounded
    graceful-termination window. *)

val list : t -> (string * status) list Lwt.t
(** [list t] is the retained jobs and their states in creation order. *)
