(** The platform seam: one internal interface, two build-time variants.

    The [select] entry of [lib/os/dune] picks exactly one implementation of this signature
    for the whole library: the POSIX one when the [charamel.os.win32] library is not
    available, the Windows one when it is. The public modules {!Charamel_os.Tty},
    {!Charamel_os.Console_input}, {!Charamel_os.Pty} and {!Charamel_os.Process} are thin,
    documented surfaces over it, which is why no other module of the library mentions
    platforms. Every operation below is described once for both variants. A POSIX build
    never compiles a line of termios-free Windows code, and a Windows build never compiles
    a line of [ioctl] or [posix_spawn].

    {!val:is_macos} is the one platform fact the pure modules need: the default browser
    differs between Darwin and the rest of POSIX. *)

val is_macos : bool
(** Whether the system is macOS. The POSIX variant reads the [sysname] field of [uname];
    the Windows variant answers [false]. *)

(** {1 Console records} *)

type key_event = {
  down : bool;
  repeat : int;
  virtual_key : int;
  wide_char : int;
  control_key_state : int;
}
(** A Win32 [KEY_EVENT_RECORD], reduced to what a VT encoder needs. [wide_char] is one
    UTF-16 code unit and is [0] when the record carries no character, which is how arrow,
    function and navigation keys arrive. *)

(** One decoded console input record.

    - [Key_event] mirrors a Win32 key record; [repeat] is its [wRepeatCount].
    - [Buffer_size] mirrors [WINDOW_BUFFER_SIZE_EVENT].
    - [Focus] mirrors [FOCUS_EVENT]: [true] when the window gained focus.
    - [Ignored] stands for every record no Charamel application can observe, notably
      [MOUSE_EVENT] and [MENU_EVENT], which the Windows reader drops. *)
type input_record =
  | Key_event of key_event
  | Buffer_size of { rows : int; cols : int }
  | Focus of bool
  | Ignored

(** {1 Terminal control} *)

module Tty : sig
  type saved
  (** A saved terminal state, produced by {!enter_raw} or {!echo_off} and consumed by
      {!restore}. Opaque because the two platforms save different things: a POSIX
      [Unix.terminal_io] snapshot, or a Windows pair of console modes plus the pair of
      active code pages. *)

  val is_stdin : bool
  (** POSIX: [Unix.isatty Unix.stdin]. Windows: whether [GetConsoleMode] succeeds on
      [GetStdHandle(STD_INPUT_HANDLE)], which is [true] for conhost and Windows Terminal
      and [false] for a redirected file or [NUL]. *)

  val is_stdout : bool
  (** As {!is_stdin} for [Unix.stdout] / [STD_OUTPUT_HANDLE]. *)

  val size_stdout : unit -> (int * int) option
  (** [(rows, cols)] of the console the standard output points at, or [None] when the
      output is not a console or the size cannot be read. POSIX issues [ioctl TIOCGWINSZ]
      on descriptor 1; Windows reads the visible window of [GetConsoleScreenBufferInfo].
      The size is read on every call, so a resize is observed without restarting the
      process. *)

  val size_of_output : [ `Stdout | `Stderr ] -> (int * int) option
  (** [(rows, cols)] of the console the selected standard output points at, or [None] when
      it is not a console or the size cannot be read. A [Unix.file_descr] is abstract and
      converts to no number in either direction, so the selection is by role, not by
      descriptor. POSIX issues [ioctl TIOCGWINSZ] on descriptor 1 or 2 after [Unix.isatty]
      accepts it; Windows reads the visible window of [GetConsoleScreenBufferInfo] for
      [GetStdHandle(STD_OUTPUT_HANDLE)] or [GetStdHandle(STD_ERROR_HANDLE)] respectively.
      Like {!size_stdout}, the size is read on every call, so a resize is observed without
      restarting the process. *)

  val enter_raw : unit -> saved
  (** Put the standard input in raw mode and return the state needed to undo it. POSIX
      applies the [cfmakeraw(3)] setting to [Unix.stdin]. Windows enables
      [ENABLE_VIRTUAL_TERMINAL_INPUT] on the input handle and
      [ENABLE_VIRTUAL_TERMINAL_PROCESSING] plus [DISABLE_NEWLINE_AUTO_RETURN] on the
      output handle, clears [ENABLE_ECHO_INPUT], [ENABLE_LINE_INPUT] and
      [ENABLE_PROCESSED_INPUT], and switches both code pages to UTF-8.
      @raise Unix_error when the standard input is not a terminal. *)

  val echo_off : unit -> saved
  (** Turn echo off, leaving everything else — canonical mode included — alone, so a
      password prompt still gets line editing. POSIX clears [ECHO] on [Unix.stdin];
      Windows clears [ENABLE_ECHO_INPUT]. Code pages are untouched.
      @raise Unix_error when the standard input is not a terminal. *)

  val restore : saved -> unit
  (** Put back the state captured by {!enter_raw} or {!echo_off}. POSIX writes the saved
      [termios] with [TCSADRAIN]. Windows restores both console modes and the code pages
      saved at entry; a mode the console rejects in the meantime is not an error, because
      the terminal is already on its way out. *)

  val controlling_input : unit -> Lwt_io.input_channel
  (** A channel over the controlling terminal, independent of the process standard input,
      so a program can keep reading keys after starting with its stdin redirected. POSIX
      opens [/dev/tty] for reading; Windows opens [CONIN$]. Each call opens a fresh
      descriptor.
      @raise Unix_error when the process has no controlling terminal. *)

  val watch_resizes : (unit -> unit) -> unit
  (** Start delivering resize notifications to the given callback. There is at most one
      watcher per process: the public {!Charamel_os.Tty} module keeps the subscriber list
      and installs this callback once. POSIX installs a [SIGWINCH] handler through
      [Lwt_unix.on_signal], so the callback runs when the signal arrives. Windows has no
      such signal: a daemon fiber polls the console size every 250 ms and calls the
      callback when it changes. *)

  val supports_suspend : bool
  (** [true] on POSIX, where [SIGTSTP] hands the terminal to the caller's shell. [false]
      on Windows, which has no job control; a suspend request there is a documented no-op.
  *)
end

(** {1 Console input records} *)

module Console : sig
  val read_records : unit -> input_record list Lwt.t
  (** Read every console input record currently queued, in arrival order. Windows performs
      [ReadConsoleInputW] on [GetStdHandle(STD_INPUT_HANDLE)] inside
      [Lwt_preemptive.detach], polling [GetNumberOfConsoleInputEvents] every 10 ms so no
      pool thread is held while nothing is typed, and answers as soon as at least one
      record is available. POSIX consoles deliver bytes, not records: the result is always
      [[]] and the call yields without blocking. *)
end

(** {1 Pseudo-terminals} *)

module Pty : sig
  type t
  (** A master/slave pseudo-terminal pair, sized at creation. Opaque: a POSIX [t] owns the
      master descriptor, and a Windows build never creates one. *)

  type error = [ `Error of string | `Unsupported ]
  (** [`Unsupported] is the whole Windows answer: a [t] cannot be built there, so no
      operation on one can be reached. *)

  val create : ?rows:int -> ?cols:int -> unit -> (t, [> error ]) result Lwt.t
  (** Allocate a pair of the given geometry, at least one row and one column. POSIX runs
      [posix_openpt] with [O_RDWR | O_NOCTTY | O_CLOEXEC], then [grantpt], [unlockpt] and
      [ptsname], and sizes the pair on the master; [rows] defaults to 24 and [cols] to
      80. The size reaches the kernel through [tcsetwinsize] where the libc exports it,
      [ioctl TIOCSWINSZ] elsewhere, and on macOS — whose ABI forbids a marshalled
      variadic [ioctl] — a spawned [stty]. Every step happens inside
      [Lwt_preemptive.detach] because [grantpt] may spawn the [pt_chown] helper.
      Windows returns [`Unsupported].
      @raise Invalid_argument when [rows] or [cols] is less than one. *)

  val slave_path : t -> string
  (** The path a process opens to attach itself to the slave side, for example
      [/dev/pts/3]. Windows raises [Invalid_argument]. *)

  val exec :
    ?cwd:string -> ?env:string array -> t -> string list -> (int, [> error ]) result Lwt.t
  (** [exec t argv] starts [argv] with the slave wired to descriptors 0, 1 and 2 and
      returns the child pid. POSIX uses [posix_spawnp] — so a program naming no directory
      separator is searched in [PATH] — with three [posix_spawn_file_actions_addopen]
      actions on {!slave_path} and [POSIX_SPAWN_SETPGROUP] and [POSIX_SPAWN_SETSID], so
      the child leads its own session and process group and takes the slave as its
      controlling terminal: never [fork], which is unsafe once [Lwt_preemptive] worker
      threads exist. An empty [argv] is rejected with [`Error]. Windows returns
      [`Unsupported]. *)

  val read : t -> int -> (string, [> error ]) result Lwt.t
  (** [read t count] returns up to [count] bytes the child wrote to its terminal. POSIX
      issues one blocking [read] on the master descriptor inside [Lwt_preemptive.detach],
      which is what makes the master usable without registering it with the event loop; a
      zero-length request answers with [[]] at once. The answer is [[]] once the child has
      closed the slave, and [`Error] on any other failure. Windows returns [`Unsupported].
  *)

  val write : t -> string -> int -> int -> (int, [> error ]) result Lwt.t
  (** [write t s off len] sends [len] bytes of [s] starting at [off] to the child's
      terminal and returns the count accepted, which may be smaller than asked for. The
      same detached blocking [write] as {!read}. Windows returns [`Unsupported].
      @raise Invalid_argument when [[off, len]] is not a range of [s]. *)

  val size : t -> (int * int, [> error ]) result
  (** [(rows, cols)] read back on the master, by the same route as the sizing in
      {!create}. Windows returns [`Unsupported]. *)

  val resize : t -> rows:int -> cols:int -> (unit, [> error ]) result
  (** Set the size as {!create} does; the kernel signals the child's foreground group
      [SIGWINCH]. Windows returns [`Unsupported]. *)

  val terminate : t -> unit
  (** [SIGKILL] the child's process group, best effort: it may already be gone. Windows
      does nothing. *)

  val close : t -> unit
  (** Release the master descriptor; safe to call twice. The slave needs no closing: this
      process never opens it. Windows does nothing. *)
end

(** {1 Processes} *)

module Process : sig
  type t
  (** A running child and the channels that reach it. Opaque: a POSIX [t] holds a pid plus
      the named pipes it created, a Windows [t] holds an [Lwt_process] object. *)

  type redir = [ `Inherit | `Null | `Pipe ]

  val spawn :
    ?cwd:string ->
    ?env:string array ->
    ?stdin:redir ->
    ?stdout:redir ->
    ?stderr:redir ->
    string list ->
    t
  (** Start a program. [argv] is the command and its arguments; a command containing no
      directory separator is looked up in [PATH] by [posix_spawnp]. The three redirections
      default to [`Inherit]. POSIX spawns with [posix_spawnp], so no [fork] happens in a
      program whose worker threads may hold runtime locks, and puts the child in its own
      process group so {!kill_tree} can reach the whole tree; [`Pipe] is wired through a
      private named pipe under a fresh subdirectory of the temporary directory, removed
      when the child is reaped. Windows spawns through [Lwt_process], whose redirection
      arguments give the same three choices natively.
      @raise Invalid_argument when [argv] is empty.
      @raise Unix_error when the program cannot be started or a redirection is refused. *)

  val pid : t -> int

  val stdin_w : t -> Lwt_io.output_channel
  (** The write end of the stdin pipe.
      @raise Invalid_argument when [~stdin] was not [`Pipe]. *)

  val stdout_r : t -> Lwt_io.input_channel
  (** As {!stdin_w} for the stdout pipe.

      @raise Invalid_argument when [~stdout] was not [`Pipe]. *)

  val stderr_r : t -> Lwt_io.input_channel
  (** As {!stdin_w} for the stderr pipe.

      @raise Invalid_argument when [~stderr] was not [`Pipe]. *)

  val await : t -> int Lwt.t
  (** Wait for the child and return its exit code: the code it exited with, or [128 + n]
      where [n] is the number of the signal that killed it — the shell convention POSIX
      callers expect. Reaping also deletes the named pipes. Calling it more than once is
      safe: the wait starts at {!spawn} and its result is shared. *)

  val signal : t -> int option Lwt.t
  (** [signal t] settles with the child, like {!await}: [Some n] when signal [n] stopped
      it and [None] when it exited on its own — disambiguating the cases {!await} folds
      into [128 + n]. *)

  val terminate : t -> unit
  (** Ask the child to stop: [SIGTERM] on POSIX, [TerminateProcess] on Windows. A child
      that already exited is not an error. *)

  val kill_tree : t -> unit
  (** Force the child and everything it started to stop. POSIX sends [SIGKILL] to the
      process group, which covers the whole tree only because {!spawn} made the child a
      group leader; when the group is gone [SIGKILL] goes to the child alone. Windows has
      no process groups, so [TerminateProcess] is applied to the child only, matching
      upstream crush: grandchildren survive there, and are documented to. *)

  val alive : t -> bool
  (** Whether the child is still running: true until the {!await} promise settles. *)
end
