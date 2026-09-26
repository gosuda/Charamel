(** Child programs: start, talk to, wait for, and stop.

    {2 POSIX}

    A child is started with [posix_spawnp] — which searches [PATH] for a program naming no
    directory separator — never with [fork]: once [Lwt_process] or any other component has
    started a [Lwt_preemptive] worker thread, forking this process would copy a runtime
    whose locks may be held by threads that do not exist in the child, and [Lwt_process]'s
    own documentation warns against exactly that combination. [posix_spawnp] is a single
    system call into libc, so the window in which that could matter does not open. The
    child is placed in a process group of its own by [POSIX_SPAWN_SETPGROUP], which is
    what gives {!val:kill_tree} something to signal.

    A [`Pipe] stream is a named pipe that this process creates under
    [Filename.temp_dir_name], opens before spawning, and hands to the child by {e path}
    through [posix_spawn_file_actions_addopen]: a descriptor number never has to cross
    between [Unix.file_descr] and a raw [int], because there is no such conversion in
    either direction. The parent opens the stdin end [O_RDWR] and the output ends
    [O_RDONLY], all [O_NONBLOCK], so no side waits for the other and the child never
    blocks in its own [open]. Because the parent's write end is [O_RDWR], the child sees
    end-of-file on its stdin when the caller closes {!val:stdin_w} rather than when the
    program exits. The pipes are deleted when the child is reaped.

    [Lwt_unix.wait4] is started as soon as the child exists, so the status is collected
    even for a caller that never awaits, and {!val:alive} and {!val:await} agree.

    {2 Windows}

    The same operations go through [Lwt_process], whose [CreateProcess]-based spawn has no
    fork to avoid and whose redirection arguments provide [Keep], [Dev_null] and private
    pipes natively. Two Windows differences are visible here, and both are documented
    rather than papered over: {!val:kill_tree} stops the child only, because there are no
    process groups (upstream crush makes the same choice), and a program name is looked up
    in [PATH] only when the executable name passed to [Lwt_process] is empty, which is
    what this module passes. *)

type t = Os_platform.Process.t
(** A running child together with the channels that reach it. *)

type redir = Os_platform.Process.redir
(** What to do with one of the child's standard descriptors. *)

val spawn :
  ?cwd:string ->
  ?env:string array ->
  ?stdin:redir ->
  ?stdout:redir ->
  ?stderr:redir ->
  string list ->
  t
(** [spawn argv] starts [argv], the program first and then its arguments, in [cwd] with
    the environment [env] — both defaulting to the current ones; [env] replaces the
    environment rather than adding to it. Each redirection defaults to [`Inherit], which
    is what a program that should simply share the parent's terminal wants, and [`Null]
    sends output to [/dev/null] or [NUL].
    @raise Invalid_argument when [argv] is empty.
    @raise Unix_error
      when the program cannot be started, including a [PATH] miss ([ENOENT]). *)

val pid : t -> int
(** The child's process id, which is also its process-group id on POSIX. *)

val stdin_w : t -> Lwt_io.output_channel
(** The writing end of the child's stdin. Closing it is how a caller says "input is over":
    the child reads end-of-file.
    @raise Invalid_argument when [~stdin] was not [`Pipe]. *)

val stdout_r : t -> Lwt_io.input_channel
(** The reading end of the child's stdout.
    @raise Invalid_argument when [~stdout] was not [`Pipe]. *)

val stderr_r : t -> Lwt_io.input_channel
(** The reading end of the child's stderr.
    @raise Invalid_argument when [~stderr] was not [`Pipe]. *)

val await : t -> int Lwt.t
(** [await t] is the child's exit code: the status it exited with, or [128 + n] when a
    signal [n] killed it — the shell convention, and the only one that survives being
    reported as an integer. Stopped children are reported the same way as signalled ones,
    since [wait4] only observes a stop when [WUNTRACED] was asked for and it is not.
    Calling it repeatedly is safe; the wait began at {!val:spawn}. *)

val terminate : t -> unit
(** Ask the child to stop: [SIGTERM] to the child itself on POSIX, [TerminateProcess] with
    exit code 1 on Windows, where [TerminateProcess] is the only one of the two that
    works. A child that has already exited is not an error. This does not touch a
    [SIGTERM]-ignoring grandchild; {!val:kill_tree} is for that. *)

val kill_tree : t -> unit
(** Force the child, and everything still in its process group, to stop with [SIGKILL]. On
    Windows, where the child is not a group leader, only the child is terminated and any
    program it started keeps running — documented, matching upstream, and the reason a
    Windows caller should prefer {!val:terminate} and an explicit child list. *)

val alive : t -> bool
(** Whether the child is still running, as far as the reaper has noticed: [true] until the
    promise {!val:await} returns settles, which means a stopped or just-exited child not
    yet collected still answers [true]. *)

val grace : float
(** {!val:stop}'s default graceful window, in seconds: the time a child is given to react
    to the first request before it is forced away. *)

val terminate_tree : t -> unit
(** Ask the child and everything in its process group to stop with [SIGTERM], ignoring a
    group that is already gone or refuses the signal. Use it when the child is a shell
    wrapper whose grandchildren must also be told to stop; {!val:terminate} reaches only
    the child. On Windows there are no process groups, so this is {!val:terminate}. *)

val group_alive : t -> bool
(** Whether the child's process group still holds a live member, tested with signal [0]. A
    group this process may not signal counts as alive. On Windows, where there are no
    groups, this is {!val:alive}. *)

val stop : ?grace:float -> t -> unit Lwt.t
(** [stop ?grace t] ends the child and everything still in its process group: it asks them
    to stop with {!val:terminate_tree}, gives the child until [grace] seconds (default
    {!val:grace}) to leave on its own, and then forces whatever is still in the group away
    with {!val:kill_tree}. The wait ends as soon as the child exits, so a child that
    leaves promptly costs nothing. It never waits for the exit status, which {!val:await}
    reports separately, and it signals nothing once the group is empty, so a recycled
    process id is never reached. *)

val stop_after_grace : ?grace:float -> t -> unit Lwt.t
(** [stop_after_grace ?grace t] is {!val:stop} for a caller that must hold the whole
    window open instead of returning as soon as the child is gone: the group is asked to
    stop, the full [grace] seconds elapse, and whatever is still in the group is then
    forced away. Use it where a deadline owns the kill and the caller measures the window
    it waited. *)
