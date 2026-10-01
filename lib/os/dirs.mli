(** Where a program keeps its files.

    These are the four directories the XDG Base Directory specification defines, the
    user's home, the temporary directory, and the expansion of a leading [~] — the
    decisions every tool in the tree otherwise re-implements. The lookup order is XDG's,
    and it is applied to Windows as well, because that is what the ecosystem's tools do
    there and what users have configured.

    A variable that is set but does not name an absolute directory is treated as unset,
    the same rule on both platforms: [XDG_CONFIG_HOME=relative/path] is broken
    configuration, and answering with a path that depends on the current directory would
    be worse than using the documented fallback.

    {2 POSIX}
    - config: [$XDG_CONFIG_HOME] else [$HOME/.config]
    - data: [$XDG_DATA_HOME] else [$HOME/.local/share]
    - state: [$XDG_STATE_HOME] else [$HOME/.local/state]
    - cache: [$XDG_CACHE_HOME] else [$HOME/.cache]
    - home: [$HOME]
    - temp: [$TMPDIR], else [/tmp]

    {2 Windows}
    - config: [$XDG_CONFIG_HOME] else [%USERPROFILE%\.config]
    - data, state, cache: [$XDG_*] else [%LOCALAPPDATA%], each holding [<app>] directly,
      since the Windows layout has no [.local] shadow of the home directory
    - home: [%USERPROFILE%], then [$HOME] for a shell that sets it
    - temp: [%TMP%] or [%TEMP%], else [%SystemRoot%\Temp]

    An application directory is the base joined with [~app]; nothing is created, because
    creating a directory is the caller's decision, not a lookup. *)

type error = [ `No_home ]
(** A home directory could not be determined. *)

val home : unit -> (string, [> error ]) result
(** [home ()] is the user's home directory, read afresh on each call so a test or an
    embedded caller can change the environment and see the result move. It is a result
    rather than an exception or an option because "which directory is this" has exactly
    two answers and the caller must handle the missing one: [Charamel_os.Dirs] no longer
    raises on an environment without [$HOME].

    Windows prefers [%USERPROFILE%] because that is what the system sets, and falls back
    to [$HOME], which a Cygwin or msys2 shell provides. *)

val config_dir : app:string -> string
(** [config_dir ~app] is where the program's configuration for [app] lives: the saved
    theme of an editor, the [settings.json] of a tool. *)

val data_dir : app:string -> string
(** [data_dir ~app] is where persistent program data goes: a database, a corpus, a log
    archive. *)

val state_dir : app:string -> string
(** [state_dir ~app] is where state that should survive a logout but not a reinstall goes:
    session files, recent items. *)

val cache_dir : app:string -> string
(** [cache_dir ~app] is where anything disposable goes, because the specification permits
    the system to delete it without asking. *)

val expand_tilde : string -> (string, [> error ]) result
(** [expand_tilde path] replaces a leading [~] with {!val:home}: [~] alone becomes the
    home directory, [~/notes.md] becomes [$HOME/notes.md], and both [~/x] and [~\\x] are
    recognized on Windows where a path arrives from a config file or a shell that used
    either separator. A path with a different leading [~] is a user-home form ([~rcfile])
    that no tool here supports, and is returned unchanged rather than guessed at; so is
    any path that does not begin with [~]. The result is [Error `No_home] only when a [~]
    had to be expanded and {!val:home} had no answer. *)

val temp_dir : unit -> string
(** [temp_dir ()] is the directory for scratch files, read on each call for the same
    reason {!val:home} is: [Filename.temp_dir_name] decides once, at initialization, and a
    caller that sets [TMPDIR] afterwards would not see it. POSIX uses [$TMPDIR] and then
    [/tmp]; Windows uses [%TMP%], then [%TEMP%], then [%SystemRoot%\Temp]. *)
