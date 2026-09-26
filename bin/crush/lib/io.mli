(** Filesystem failures in the shape the tools report them.

    Two jobs, both small: turn an exception from a file operation into the error value a
    caller already returns, and read a file that must not be allowed to fill memory. The
    error vocabulary is the one the tools already carry — a missing path and an I/O
    failure that names the path and says what went wrong.

    {2 What is trapped}

    Only failures of the {e filesystem} are converted: [Unix_error],
    {!exception:
    Charamel_os.Fs.E} and [Sys_error]. Anything else — a cancelled
    promise, a bug — is re-raised so it reaches its own handler rather than being reported
    as a disk error. [ENOTDIR] counts as missing: a path that walks through a file has no
    directory to look in. *)

type error = [ `Io of string * string | `Not_found of string ]
(** The failures a trapped operation reports: [Not_found target] for a missing path,
    [Io (target, message)] for anything else the filesystem refused. *)

val fs_error : Charamel_os.Fs.error -> string
(** [fs_error error] is the lower-case name callers show for [error]. *)

val message : exn -> string
(** [message exn] describes a filesystem exception for a user: the errno name with the
    function and argument that raised it, the name of a {!exception:Charamel_os.Fs.E} with
    its path, the text of a [Sys_error], and [Printexc.to_string] otherwise. *)

val trap : string -> (unit -> 'a Lwt.t) -> ('a, [> error ]) result Lwt.t
(** [trap target operation] runs [operation ()] and wraps its result: [Ok] on success,
    [Error (`Not_found target)] when it reports a missing path, and
    [Error (`Io (target, message exn))] for any other filesystem failure. A non-filesystem
    exception is re-raised. [target] is the path the caller names in its own error type —
    the tool-relative name, not necessarily the one that was opened. *)

val trap_await : string -> (unit -> 'a Lwt.t) -> ('a, [> error ]) result
(** [trap_await target operation] is {!val:trap} for a direct-style caller: it starts
    [operation] and waits for the outcome. Use it only where the surrounding code already
    awaits, never inside a [Lwt_preemptive] thread. *)

val read_bounded : string -> max:int -> string option Lwt.t
(** [read_bounded path ~max] is the content of [path], or [None] when the file cannot be
    opened, is unreadable, or holds more than [max] bytes. The read stops as soon as the
    limit is passed, so an oversized file is never buffered. *)

val lines : string -> string list
(** [lines text] splits [text] at newlines and drops a carriage return left at the end of
    a line, so CRLF and LF input parse the same way. *)
