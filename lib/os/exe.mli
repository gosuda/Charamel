(** Finding an executable.

    {2 POSIX}

    [PATH] entries are separated by [:], and a candidate must be an existing regular file
    that [access(X_OK)] accepts, so a directory on the path and a non-executable data file
    are both rejected.

    {2 Windows}

    [PATH] entries are separated by [;], and the extension matters: typing [git] means
    [git.exe], and [npm] means [npm.cmd]. [PATHEXT] supplies the ordered extension list,
    defaulting to [.COM;.EXE;.BAT;.CMD], and a name that already carries one of those
    extensions is tried as it stands before the list is consulted. A candidate must exist;
    the executable-bit question [access(X_OK)] raises does not exist on this platform. *)

val find : ?win32:bool -> string -> string option
(** [find ?win32 name] is the executable [name] resolves to, or [None] when nothing on
    [PATH] answers. A [name] containing a directory separator is a path, not a lookup: it
    is checked as given (plus the extension list on Windows) and never joined onto a
    [PATH] entry, which is the rule the shells use and the one that makes [./build/tool]
    findable. An empty [name] is [None]. [PATH] and [PATHEXT] are read on every call, so a
    caller that changes the environment sees the change. [~win32] selects the platform
    rules and defaults to the host, so a test on one system can pin the rules of the
    other. *)

val for_spawn : ?win32:bool -> string -> string
(** [for_spawn ?win32 prog] is the program name the spawn funnel hands to the system: a
    bare name is resolved through {!val:find} to the absolute path it answers, because the
    funnel passes that name as the application name and a bare name misses there. A name
    that already carries a directory separator and the empty program go through unchanged,
    as does a miss, so the system reports an unknown program exactly as before. *)
