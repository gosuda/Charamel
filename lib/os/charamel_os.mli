(** The platform seam: what changes between POSIX and Windows, in one place.

    Charamel renders a terminal, reads keys, spawns programs and finds directories.
    Everything else in the tree can be written once because these eleven modules absorb
    the differences, and each of them hides exactly one decision: {!Charamel_os.Tty} what
    a terminal is, {!Charamel_os.Console_input} how Windows key events become bytes,
    {!Charamel_os.Pty} how a child gets a terminal of its own, {!Charamel_os.Process} how
    a program is started without forking a threaded runtime, {!Charamel_os.Dirs} where
    files belong, {!Charamel_os.Exe} which file is the program, {!Charamel_os.Shell} how
    text reaches a shell, {!Charamel_os.Editor} which program the user asked for,
    {!Charamel_os.Signal} which signals can be caught, {!Charamel_os.Fs} how a file
    operation fails, and {!Charamel_os.Time} how long to wait.

    A program never chooses between platforms: dune's [select] picks one implementation of
    an internal interface at build time, so a POSIX binary contains no Windows code and a
    Windows binary no [ioctl]. The parts that are pure decisions — directory precedence,
    word splitting, the shape of an error — are written once and are unit-tested on every
    platform, and the parts that are not are the only code that knows the difference.

    {2 Concurrency}

    Everything asynchronous here runs on Lwt. The two places a blocking system call is
    unavoidable — a pseudo-terminal master, which cannot be registered with the event
    loop, and the Windows console input queue, which has no waitable handle — use
    [Lwt_preemptive.detach], and the module documentation names that cost. No module here
    calls [Lwt_main.run], forks a child, or starts a thread of its own. *)

module Time = Time
module Tty = Tty
module Console_input = Console_input
module Pty = Pty
module Process = Process
module Dirs = Dirs
module Exe = Exe
module Shell = Shell
module Editor = Editor
module Signal = Signal
module Fs = Fs
