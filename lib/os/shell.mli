(** Handing a string to a shell.

    {2 POSIX}

    A command runs under [/bin/sh -c], so pipelines, redirections, globs and variable
    expansion all mean what they mean in a script.

    {2 Windows}

    A command runs under [cmd.exe /d /s /c]: [/d] suppresses the [AutoRun] registry keys
    so a user's shell configuration cannot change what the program executes, and [/s] is
    what makes [cmd.exe] keep the quoting this module adds instead of stripping the
    outermost pair.

    Both platforms embed a requested working directory as a leading [cd] rather than
    leaving it for the caller to apply, because the point of {!val:command} is to hand
    back one argv list that is enough on its own:
    [Process.spawn (Shell.command ~cwd text)].

    No part of either shell's job-control or profile behavior is relied on, and nothing
    here pretends the other shell exists: [quote] on POSIX is single-quoting, on Windows
    it is the caret-escaping [cmd.exe] needs, and passing text through the wrong one is a
    broken command line, not a portable one. *)

val command : ?cwd:string -> string -> string list
(** [command ?cwd text] is the argv list that runs [text] through the platform shell —
    ["/bin/sh"; "-c"; text] on POSIX, [cmd.exe; "/d"; "/s"; "/c"; text] on Windows. With
    [~cwd], the text is prefixed by a change of directory ([cd 'dir' && …] on POSIX,
    [cd /d dir && …] on Windows) so the single command has everything it needs. The
    returned list is arguments, not a string to be re-parsed: pass it to
    {!Charamel_os.Process.spawn}. *)

val quote : string -> string
(** [quote text] wraps [text] so a shell treats it as one word. POSIX single-quotes it and
    closes-and-reopens around an embedded quote, which is the only escaping that works
    inside single quotes and holds for every byte including newline and [NUL]-adjacent
    controls. Windows double-quotes it and caret-escapes the metacharacters [cmd.exe]
    would otherwise act on. *)

val split_words : string -> string list
(** [split_words text] divides [text] into words on spaces and tabs, honoring single and
    double quotes, which is what a program name and its arguments look like when they
    arrive from [$EDITOR] or a configuration file. An unterminated quote ends at the end
    of the input rather than raising, because a half-written environment variable should
    not stop a program; a quote character inside a word starts quoting rather than
    escaping, matching the behavior every other tool in this tree already has. *)
