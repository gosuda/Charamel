(** Bounded background process jobs.

    Jobs run shell commands under one Eio switch, capture merged standard output, and
    retain a finite completed-job history. *)

type t
(** The type for a job registry. *)

val create :
  sw:Eio.Switch.t ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  artifacts:Artifact.t ->
  t
(** [create ~sw ~proc_mgr ~clock ~artifacts] is an empty registry attached to [sw]. Child
    processes are cancelled with [sw]. *)

val start :
  t ->
  cwd:string ->
  command:string ->
  env:(string * string) list ->
  timeout_s:int ->
  string
(** [start t ~cwd ~command ~env ~timeout_s] starts [command] through the POSIX shell in
    [cwd] with [env] overrides. [timeout_s] is the maximum runtime in seconds and must be
    positive. It returns a fresh identifier of the form [job-N]. Standard output and
    standard error are captured together with a ten-megabyte bound. *)

type status =
  | Running
  | Exited of int
  | Killed  (** The type for a job lifecycle state. *)

val output :
  t -> id:string -> wait:bool -> (string * status, [ `Not_found of string ]) result
(** [output t ~id ~wait] is the captured output and current status for [id]. When [wait]
    is [true], it waits for completion for at most ten minutes. A timed-out wait returns
    the output collected so far with [Running]. *)

val kill : t -> id:string -> (unit, [ `Not_found of string ]) result
(** [kill t ~id] requests graceful termination of [id], then schedules a forced
    termination after two seconds if the process is still running. *)

val kill_all : t -> unit
(** [kill_all t] requests termination of every running job and waits through the bounded
    graceful-termination window. *)

val list : t -> (string * status) list
(** [list t] is the retained jobs and their states in creation order. *)
