(** Lexical file paths: one normalizer, one containment test, one parent step.

    These operations read a path as text only. They never touch the filesystem, so a path
    that does not exist, or whose parent is a file, still normalizes, and a symbolic link
    is followed only in the sense that its name is spelled out. A caller that must resolve
    links asks the operating system ([Tool.canonical]).

    The two rules that matter to a security decision are that a result is always absolute
    once its input was, and that a [..] never climbs past the root it sits on: the walk
    clamps there instead of cancelling the root away. Every production caller already
    hands in an absolute [~cwd] and an absolute [~root].

    Each operation takes the platform as an optional [~windows] argument and defaults it
    to the host, so a test on one system can pin the rules of the other. *)

val is_absolute : ?windows:bool -> string -> bool
(** [is_absolute ~windows path] is [true] when [path] names a root of its own: a leading
    separator, or on Windows a drive letter such as [C:] and a UNC prefix [//server]. *)

val normalize : ?windows:bool -> ?cwd:string -> string -> string
(** [normalize ?windows ?cwd path] resolves [path] against [cwd] when [cwd] is given and
    [path] is relative, then removes every empty and [.] component, applying each [..] to
    the component before it. A [..] that would climb past the root is dropped, so the
    result of an absolute input is never shorter than that root. On Windows a backslash is
    a separator, a drive letter is kept and upper-cased, and a path that starts at the
    root of the current drive takes [cwd]'s drive letter — including the [/C:/x] form a
    file URI carries. A trailing separator disappears.

    - An absolute [path] ignores [cwd].
    - A relative [path] with [cwd] resolves against it; without [cwd] it stays relative.
    - The empty [path] is [cwd] normalized, or [.] with no [cwd].
    - A normalized absolute path with nothing left is its root: [/], [C:/], or
      [//server/share/].
    - A normalized relative path with nothing left is [.]. *)

val parent : ?windows:bool -> string -> string option
(** [parent ~windows path] is [path] with its final component removed, normalized. It is
    [None] when there is nowhere left to go: the filesystem root, a drive root, a UNC
    share root, or a relative path of one component. A caller walking up a directory tree
    stops on [None] rather than comparing against a literal root, which is what keeps a
    walk on [C:/] from repeating forever. *)

val within : ?windows:bool -> root:string -> string -> bool
(** [within ~windows ~root path] is [true] when the normalized [path] equals or lies
    beneath the normalized [root] at a component boundary. Both sides must be absolute and
    must name the same root — the same drive, the same UNC share — so [D:/a] is not within
    [C:/a] and [/a/bc] is not within [/a/b]. A [root] of [/] contains every absolute path
    on that root. When either side is relative the answer is [false]: a caller comparing a
    path it cannot place must treat it as outside, which is the fail-closed direction for
    a permission test. *)

val relative : ?windows:bool -> root:string -> string -> string option
(** [relative ~windows ~root path] is [path] expressed from [root] — the components after
    [root]'s, joined by [/] — or [None] when {!val:within} does not hold. [path] equal to
    [root] gives [""]. *)

val under : ?windows:bool -> root:string -> string -> string
(** [under ~root path] resolves [path] inside a sandbox [root]: relative paths join [root]
    as-is, and an absolute [path] keeps only its components — the mirror of [C:\a\b] under
    [root] is [root\a\b], the shape POSIX gets from plain concatenation. A bare [root]
    ("/" or a drive root) names the real filesystem and absolute paths pass through. *)
