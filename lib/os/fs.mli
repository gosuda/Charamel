(** File operations with the error names callers already match on.

    These are [Lwt_unix] and [Unix] operations with an error vocabulary attached: a failed
    lookup, a collision, a refusal, a directory where a file was wanted. The four names
    {!type:error} collects are the ones callers in this tree already match against, so a
    migration keeps its match arms; anything outside that set is a genuine surprise and is
    {e raised} rather than folded into a value, because reporting [Permission_denied] for
    [ENAMETOOLONG] would be a lie that a caller cannot act on.

    {2 POSIX}

    [rename_replace] is atomic: [rename] replaces an existing destination within one
    filesystem. [read_dir] lists a directory. [mkdir_p] creates every missing component.

    {2 Windows}

    The same operations behave the same way because OCaml's [Unix] layer maps them onto
    the Win32 calls that have the equivalent semantics: [MoveFileEx] with the replace
    flag, which is what [Unix.rename] issues, so replacing an existing file works rather
    than failing as the raw C [rename] would. Directory listing and creation go through
    the same [readdir] and [mkdir] entry points. Nothing here assumes case-sensitivity,
    symlinks, or the [X_OK] bit, all three of which Windows may not offer. *)

type error = [ `Already_exists | `Is_directory | `Not_found | `Permission_denied ]
(** The failures a caller is expected to handle. *)

exception E of error * string
(** [E (error, path)] reports an error outside {!type:error} that still belongs to [path],
    and the errno that produced it. Raised by the operations whose result type has no room
    for it — {!val:rename_replace} and {!val:with_open_out} — and by any operation that
    hits a failure the taxonomy does not name. *)

val rename_replace : src:string -> dst:string -> unit Lwt.t
(** [rename_replace ~src ~dst] moves [src] onto [dst], replacing whatever is there. The
    pair is the safe way to publish a file: write to a temporary name, then rename, so a
    reader sees either the old content or the new, never a half-written file.
    @raise E
      with [Not_found] when [src] or its directory is missing, [Permission_denied] when
      the directory refuses, and [Is_directory] when either side names a directory. *)

val with_open_out :
  perm:int -> string -> (Lwt_io.output_channel -> unit Lwt.t) -> unit Lwt.t
(** [with_open_out ~perm path k] truncates or creates [path] with mode [perm], hands the
    channel to [k], and closes the channel afterwards — including when [k] raises or its
    promise fails, so a half-written file is not left open on the way out. The parent
    directory must already exist; a caller that creates directories calls {!val:mkdir_p}
    first, because silently making a directory the caller did not ask for is a worse
    surprise than an error.
    @raise E when the file cannot be opened. *)

val read_dir : string -> (string list, string) result Lwt.t
(** [read_dir path] is the names inside [path], excluding the self and parent entries, in
    the order the system reports them. The error is a message string, because listing a
    directory fails for reasons as varied as its contents and callers report it verbatim.
*)

val read_link : string -> (string, [> error ]) result Lwt.t
(** [read_link path] is the target recorded in the symbolic link at [path]. [Not_found]
    also covers the case where [path] exists but is not a link, which is what a caller
    asking "where does this link point" needs to know either way. *)

val stat : string -> (Unix.stats, [> error ]) result Lwt.t
(** [stat path] is the metadata for [path], following a symbolic link. The result is
    [Unix.stats] rather than a translation: the fields a caller reads — [st_kind],
    [st_size], [st_mtime] — are the same on both platforms. *)

val unlink : string -> (unit, [> error ]) result Lwt.t
(** [unlink path] removes the file or link named [path], never a directory —
    [Is_directory] is the answer when one is asked for, and the caller decides whether to
    remove a tree. *)

val mkdir_p : string -> (unit, [> error ]) result Lwt.t
(** [mkdir_p path] creates [path] and every missing parent, and succeeds when the
    directory already exists, since "make sure this exists" is what callers mean. It
    reports [Already_exists] when a component exists but is not a directory: the path is
    occupied by something else, and no amount of retrying changes that. *)
