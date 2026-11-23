(** Finding an executable.

    {2 POSIX}

    [PATH] entries are separated by [:], and a candidate must be an existing regular file
    that [access(X_OK)] accepts, so a directory on the path and a non-executable data file
    are both rejected.

    {2 Windows}

    [PATH] entries are separated by [;], and the extension matters: typing [git] means
    [git.exe], and [npm] means [npm.cmd]. [PATHEXT] supplies the ordered extension list,
    defaulting to [.COM;.EXE;.BAT;.CMD], and a name that already carries one of those
    extensions is tried as it stands. An explicitly named batch file ([.BAT] or [.CMD] in
    any case) is likewise tried as it stands whatever [PATHEXT] lists, because the
    launcher runs it through the command interpreter. Otherwise each extension is tried in
    [PATHEXT] order before the name as written, because the extensionless file beside a
    package shim is its POSIX shell script, which the process launcher cannot start. A
    candidate must exist; the executable-bit question [access(X_OK)] raises does not exist
    on this platform. *)

val find : ?win32:bool -> string -> string option
(** [find ?win32 name] is the executable [name] resolves to, or [None] when nothing on
    [PATH] answers. A [name] containing a directory separator is a path, not a lookup, as
    on Windows is a drive-letter prefix such as [C:tool.exe]: it is checked as given (plus
    the extension list on Windows) and never joined onto a [PATH] entry, which is the rule
    the shells use and the one that makes [./build/tool] findable. An empty [name] is
    [None]. A hit is always an absolute path, except a Windows drive-relative one such as
    [C:tool.exe], which is returned as given: only the system can resolve it against that
    drive's working directory. A [PATH] entry spelled relative to the current directory is
    read from there. [PATH] and [PATHEXT] are read on every call, so a caller that changes
    the environment sees the change. [~win32] selects the platform rules and defaults to
    the host, so a test on one system can pin the rules of the other. *)

val for_spawn : ?win32:bool -> string -> string
(** [for_spawn ?win32 prog] is the program name the spawn funnel hands to the system: a
    bare name that names a runnable file in the current directory resolves there first,
    because that is where the system reads it from, and otherwise through {!val:find} to
    the absolute path it answers, because the funnel passes that name as the application
    name and a bare name misses there. A name that already carries a directory separator,
    a Windows drive-letter prefix such as [C:tool.exe], and the empty program go through
    unchanged, as does a miss, so the system reports an unknown program exactly as before.
*)

val command_line : string list -> string
(** [command_line argv] is the Windows command line the spawn funnel hands to
    [CreateProcess] for [argv], the program first. Each word is written the way the C
    runtime splits it back ([CommandLineToArgvW]): quoted only when it is empty or holds
    whitespace or a quote, with an inner quote escaped by a backslash and the backslashes
    before an escaped or closing quote doubled. A program named [cmd] or [cmd.exe] (in any
    case, with or without a directory) reads its command line differently: its switches go
    through as written, and the words after [/c] or [/k] are command text, joined by
    spaces verbatim inside the one pair of quotes [cmd.exe] removes, so
    [["cmd"; "/c"; "echo a & echo b"]] runs both commands. Without [/s], [cmd.exe] keeps
    that pair when its text is exactly the name of an executable file with a space in it,
    which still runs that file. The empty [argv] is the empty line. *)

val spawn_command : ?win32:bool -> string list -> string * string
(** [spawn_command ?win32 argv] is the application name and the command line the spawn
    funnel hands to [CreateProcess]: {!val:for_spawn} of the program and
    {!val:command_line} of [argv]. A program that resolves to an existing batch file
    ([.BAT] or [.CMD]) is the exception, because [CreateProcess] would run it through
    [cmd /c] over the command line, which looks the first word up again from the child's
    working directory and reads [&] in an argument as the end of the command. The
    application is then [cmd.exe] from [PATH], and the line is
    [cmd.exe /e:ON /v:OFF /d /c ""script" args"] with the absolute script path: an
    argument that holds more than letters, digits and [#$*+-./:?@\_] is quoted, an inner
    quote doubles, the backslashes before a quote or the closing quote double, and each
    [%] is written [%%cd:~,%], which [cmd.exe] reads back as one [%] instead of expanding
    a variable. A batch file that does not resolve goes through unchanged, so the system
    reports it exactly as before. The empty [argv] is [("", "")].
    @raise Invalid_argument
      when a batch file argument holds a line break, which would end the command text. *)
