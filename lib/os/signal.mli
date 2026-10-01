(** Which signals a process may ask about.

    {2 POSIX}

    Every signal the platform defines is supported, including the three that matter to a
    terminal application: [SIGWINCH] for a resized window, [SIGTSTP] to suspend to the
    shell, and [SIGCONT] to come back.

    {2 Windows}

    There is no signal machinery to install. The C runtime offers [raise] for a handful of
    numbers, so a handler for [SIGINT] or [SIGTERM] can be registered and will fire when
    the console hands the process a [CTRL+C], but [SIGWINCH], [SIGTSTP] and [SIGCONT] have
    no counterpart at all: nothing generates them, and OCaml's [Sys.set_signal] rejects
    them. A caller that installs one unconditionally gets a failure at startup instead of
    a program that simply cannot be suspended, so this module is how
    {!Charamel_os.Tty.supports_suspend} and the tea runtime decide what to ask for. *)

val supported : int -> bool
(** [supported signal] is whether this platform can deliver [signal], where the argument
    is a platform number as returned by [Sys.signal_to_int]. Every signal is supported on
    POSIX. On Windows, [SIGWINCH], [SIGTSTP] and [SIGCONT] answer [false] and everything
    else answers [true]: the answer is about delivery, so a signal the runtime would
    accept but nothing would ever send is still reported as supported. *)
