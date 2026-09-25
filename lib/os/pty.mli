(** Pseudo-terminals: a child that believes it owns a terminal.

    Three tools need this: capturing what a program paints without a terminal in the loop,
    running a shell command so that it keeps its interactive behavior, and handing a
    remote SSH session a terminal of its own. All three need a device the child can treat
    as its controlling terminal, which a pipe is not: [isatty] on a pipe says no.

    {2 POSIX}

    A pair comes from [posix_openpt] with [O_NOCTTY | O_CLOEXEC], then [grantpt] and
    [unlockpt], and its name from [ptsname]. The master descriptor is a raw [int] held by
    the opaque {!type:t}: it never becomes a [Unix.file_descr], because there is no
    conversion in either direction, so {!val:read} and {!val:write} issue one blocking
    system call each inside [Lwt_preemptive.detach] instead of registering with the event
    loop. A concurrent reader therefore occupies one pool worker until data arrives; that
    is the cost of a master descriptor the scheduler cannot poll, and the reason
    {!val:read} takes a count rather than handing out a channel. {!val:create},
    {!val:exec} and {!val:resize} run the same way for the same reason, and [grantpt] in
    particular may itself spawn the [pt_chown] helper.

    The child is started by [posix_spawnp] — so a program name with no directory separator
    is searched in [PATH] — never by [fork]:
    {e forking a process that has already started [Lwt_preemptive] worker threads is
       unsafe}, and the child receives the slave as descriptors 0, 1 and 2 through three
    [addopen] actions on {!val:slave_path}, so no descriptor number has to cross the
    boundary. [POSIX_SPAWN_SETSID] also makes the child a session leader, and because a
    session leader with no controlling terminal that opens a terminal slave takes it as
    its own, the slave becomes the child's controlling terminal: [Ctrl+C] and [Ctrl+Z]
    reach it, [TIOCSWINSZ] signals its foreground group, and [isatty] tells the truth.
    Leading its own process group is what lets {!val:terminate} reach everything it
    started.

    {2 Windows}

    There is no answer. [CreatePseudoConsole] yields a handle a ConPTY relay consumes, not
    a device a [CreateProcess] child can put on its standard handles, so every function
    here returns or reports [`Unsupported] and callers keep the documented Windows
    behavior of running the program without a terminal — the same restriction upstream
    imposes on itself. *)

type t = Os_platform.Pty.t
(** An open master/slave pair, sized as requested. *)

type error = Os_platform.Pty.error
(** [`Error] carries a system message; [`Unsupported] is the only answer a Windows build
    gives. *)

val create : ?rows:int -> ?cols:int -> unit -> (t, [> error ]) result Lwt.t
(** [create ()] allocates a pair, [rows] by [cols], defaulting to 24 by 80. The size is
    set with [ioctl TIOCSWINSZ] on the master before any child attaches, so the child's
    first [TIOCGWINSZ] already sees the requested geometry. *)

val slave_path : t -> string
(** The path the child opens — [/dev/pts/3] on Linux, [/dev/ttys001] on macOS — which is
    also what a caller passes to a program that takes a [--tty] argument.
    @raise Invalid_argument on Windows, where no [t] exists to ask. *)

val exec :
  ?cwd:string -> ?env:string array -> t -> string list -> (int, [> error ]) result Lwt.t
(** [exec t argv] starts [argv] — program first, then arguments, looked up in [PATH] when
    it contains no directory separator — with the slave on descriptors 0, 1 and 2, and
    returns the child's pid. The child becomes a session and process-group leader, and the
    slave becomes its controlling terminal. [env] replaces the environment entirely rather
    than adding to it, and defaults to the current one. The master stays open on this
    side: the kernel reports end of input on it once the last slave reference goes away,
    which is how {!val:read} notices the child exiting. A failed [posix_spawnp] becomes
    [Error (`Error ..)]; nothing is raised except the range check in {!val:write} and the
    geometry check in {!val:create}. *)

val read : t -> int -> (string, [> error ]) result Lwt.t
(** [read t count] returns whatever the child has written to its terminal, up to [count]
    bytes, blocking until at least one byte arrives. It returns [[]] when the child has
    closed the slave or exited — the [EIO] a POSIX master read reports at that moment is
    end-of-input here, not an error — and a non-positive [count] answers [[]] without
    blocking at all. *)

val write : t -> string -> int -> int -> (int, [> error ]) result Lwt.t
(** [write t text off len] sends the range [text, off, len] to the child's terminal and
    returns how many bytes the device accepted, which may be fewer than asked for when the
    line discipline is behind; a caller loops from the returned count.
    [Error `Unsupported] is the Windows answer.
    @raise Invalid_argument when [[off, len]] is not a range of [text]. *)

val size : t -> (int * int, [> error ]) result
(** [size t] is the current [(rows, cols)] read back from the master with
    [ioctl TIOCGWINSZ], which is the geometry the child sees. *)

val resize : t -> rows:int -> cols:int -> (unit, [> error ]) result
(** [resize t ~rows ~cols] changes the geometry with [ioctl TIOCSWINSZ]; the kernel
    signals the child's foreground group [SIGWINCH], so a program that subscribes to
    {!Charamel_os.Tty
    .on_resize} for its own terminal, or reads [SIGWINCH] directly,
    re-reads and repaints. The new size is remembered for {!val:size}. *)

val terminate : t -> unit
(** [SIGKILL] the process group of the most recent {!val:exec}, if there was one. A group
    that has already exited is not an error, and a second {!val:exec} replaces the target:
    a [t] terminates one child, the last one started. Does nothing on Windows. *)

val close : t -> unit
(** Release the master descriptor. Idempotent, because a caller closing from a cleanup
    path and from a finaliser is normal. The slave needs nothing from us: this process
    never opens it, and holding it open here would prevent {!val:read} from ever seeing
    end of input. Does nothing on Windows. *)
