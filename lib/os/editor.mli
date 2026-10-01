(** The programs a user reads or edits in.

    These are the three external programs an interactive tool reaches for, resolved the
    way a shell would resolve them: from the environment when it says something, from the
    platform default when it does not, and split into a program and its arguments so the
    result goes straight into {!Charamel_os.Process.spawn}.

    {2 POSIX}

    [$EDITOR], else [vi]; [$PAGER], else [less -r]; [$BROWSER], else [open] on macOS and
    [xdg-open] everywhere else.

    {2 Windows}

    [notepad], [more], and [cmd /c start] — the last one a two-element prefix rather than
    a program, because [start] is a [cmd.exe] internal and no executable of that name
    exists to spawn.

    An environment variable that is set but blank, or blank after trimming, counts as
    absent: [EDITOR=] in a container means "nobody chose an editor", not "run the empty
    program". *)

val editor : unit -> string list
(** [editor ()] is the line editor to hand a file to. [$VISUAL] is not consulted: the
    tools this serves ask for one editor, and two variables that may disagree is a
    question with no good answer. *)

val pager : unit -> string list
(** [pager ()] is the program that displays text a screen at a time. [less] gets the
    {e -r} option so the ANSI sequences a renderer emits stay sequences rather than
    becoming literal text, which is the difference between a highlighted document and one
    full of caret notation. *)

val browser : unit -> string list
(** [browser ()] is the prefix that opens a URL. macOS gets [open], Windows gets
    [cmd /c start], and every other POSIX system gets [xdg-open]; [$BROWSER] overrides all
    three, word-split the same way [$EDITOR] is. This is also the fix for the tool that
    asked a Mac to run [xdg-open]: the default now follows the platform, so the URL opens
    without the user setting anything. *)
