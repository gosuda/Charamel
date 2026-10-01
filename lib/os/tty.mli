(** The local terminal: detection, size, modes, and resize notification.

    Every function here talks about the process's own standard input and output; a
    terminal reached over a socket is {!Charamel_tea.Terminal}'s problem, not this
    module's. The POSIX implementation uses [Unix.tcgetattr]/[Unix.tcsetattr] and [ioctl];
    the Windows implementation uses [GetConsoleMode], [SetConsoleMode] and
    [GetConsoleScreenBufferInfo] through ctypes, and never converts between a
    [Unix.file_descr] and a [HANDLE].

    {1 Detection and size} *)

val is_tty_stdin : bool
(** Whether standard input is a terminal. POSIX: [Unix.isatty Unix.stdin]. Windows:
    whether [GetConsoleMode] accepts the standard input handle, which is true for conhost
    and Windows Terminal and false for a redirected file, [NUL], or a pipe. Evaluated
    once, at module initialization. *)

val is_tty_stdout : bool
(** Whether standard output is a terminal, on the same terms as {!val:is_tty_stdin}. *)

val size_stdout : unit -> (int * int) option
(** [size_stdout ()] is the current [(rows, cols)] of the terminal on standard output, or
    [None] when it is not a terminal or the size cannot be read. The size is asked for on
    every call rather than cached, because a window can be resized at any moment; a caller
    that must react to a change subscribes with {!val:on_resize} and re-reads.

    POSIX issues [ioctl TIOCGWINSZ] on descriptor 1, the number [Unix.stdout] always has.
    Windows reads the visible window of [GetConsoleScreenBufferInfo] for the standard
    output handle. *)

val size_of_output : [ `Stdout | `Stderr ] -> (int * int) option
(** [size_of_output output] is the current [(rows, cols)] of the terminal the selected
    standard output points at, or [None] when it is not a terminal or the size cannot be
    read — the same contract as {!val:size_stdout}, for the descriptor the caller actually
    renders to, which is [Stderr] for a tool that keeps stdout for data. The selection is
    by role, not by [Unix.file_descr]: that type is abstract and converts to no number in
    either direction. POSIX issues [ioctl TIOCGWINSZ] on descriptor 1 or 2; Windows reads
    the screen buffer of [GetStdHandle(STD_OUTPUT_HANDLE)] or
    [GetStdHandle(STD_ERROR_HANDLE)]. *)

(** {1 Modes} *)

val enter_raw : unit -> unit -> unit
(** [enter_raw ()] puts standard input into raw mode and returns the closure that puts it
    back. No echo, no line buffering, no signal interpretation: every keystroke arrives as
    itself, which is what a keyboard-driven interface needs, and the returned closure
    restores the exact state captured on entry, so nested or repeated uses do not lose the
    user's settings.

    POSIX applies the [cfmakeraw(3)] setting to [Unix.stdin] with [TCSANOW] and restores
    with [TCSADRAIN], which waits for output to drain first. Windows clears
    [ENABLE_ECHO_INPUT], [ENABLE_LINE_INPUT] and [ENABLE_PROCESSED_INPUT] and sets
    [ENABLE_VIRTUAL_TERMINAL_INPUT] on the input handle, sets
    [ENABLE_VIRTUAL_TERMINAL_PROCESSING] and [DISABLE_NEWLINE_AUTO_RETURN] on the output
    handle, and switches both code pages to UTF-8 ([65001]); the restore closure writes
    the saved modes and the saved code pages back, in that order, ignoring a mode the
    console rejects in the meantime because the terminal is already being given up.

    The closure is idempotent in the sense that restoring twice is harmless, but it is not
    safe to interleave: the state captured at entry is what comes back.
    @raise Unix_error
      when standard input is not a terminal ([ENOTTY]). Call {!val:is_tty_stdin} first, or
      fall back to a non-interactive mode. *)

val echo_off : unit -> unit -> unit
(** [echo_off ()] turns echo off and returns the closure that turns it back on, leaving
    canonical mode and signal handling intact: a password prompt gets its line editing and
    its [CTRL+C]. This is the mode to use when the caller is not driving the screen, only
    hiding what it types.

    POSIX clears [ECHO] on [Unix.stdin]; Windows clears [ENABLE_ECHO_INPUT] and touches
    neither the output handle nor the code pages.
    @raise Unix_error when standard input is not a terminal. *)

(** {1 The controlling terminal} *)

val open_controlling_in : unit -> Lwt_io.input_channel
(** [open_controlling_in ()] is a channel over the process's controlling terminal,
    independent of standard input, so a program started with its stdin redirected —
    [charm | tee log], or a [gum] prompt inside a pipeline — can still read keys. POSIX
    opens [/dev/tty]; Windows opens [CONIN$], which always refers to the console input
    buffer.

    Each call opens a new descriptor and returns a channel that owns it, so the caller
    closes it; the channel is not shared with {!val:echo_off} or {!val:enter_raw}, which
    address standard input.
    @raise Unix_error
      when the process has no controlling terminal ([ENXIO] on POSIX, when [CONIN$] cannot
      be opened on Windows). *)

(** {1 Resize notification} *)

val on_resize : (unit -> unit) -> (unit -> unit) Lwt.t
(** [on_resize callback] subscribes [callback] to resize notifications and resolves, once
    the subscription is live, with the function that removes it. Subscriptions are
    process-wide and independent of any one program: the first subscriber installs the
    watcher, the last one to unsubscribe leaves it in place, and a callback that raises is
    reported through [Lwt.async_exception_hook] rather than breaking the others.

    POSIX installs one [Lwt_unix.on_signal] handler for [SIGWINCH]; the kernel sends it to
    the foreground process group when the window changes, so a notification means "re-read
    {!val:size_stdout}", not the new size. Windows has no such signal, so one daemon fiber
    polls the console size every 250 ms and calls the subscribers when it differs from the
    last reading, which makes a Windows notification arrive up to that long after the
    resize and makes a missed change impossible. *)

(** {1 Job control} *)

val supports_suspend : bool
(** Whether suspending the program to the shell means anything here. [true] on POSIX,
    where [SIGTSTP] stops the process and [SIGCONT] resumes it, with the caller expected
    to leave raw mode first. [false] on Windows, which has no job control: a caller must
    treat a suspend request as a no-op, and {!Charamel_os.Signal} will not accept the
    signals involved. *)
