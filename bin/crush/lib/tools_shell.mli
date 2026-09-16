(** Shell and background-job tools.

    Shell commands run through the local process capability. Background jobs are retained
    by the shared job registry and can be queried or terminated. *)

val is_read_only : string -> bool
(** [is_read_only command] is [true] only when [command] has a statically safe, read-only
    shell form. Commands whose shell grammar cannot be proved safe are classified as
    mutable. *)

val bash : Tool.t
(** [bash] executes a shell command in the requested working directory. *)

val job_output : Tool.t
(** [job_output] returns captured output and status for a background job. *)

val job_kill : Tool.t
(** [job_kill] terminates a background job. *)
