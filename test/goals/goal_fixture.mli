(** Shared fixtures for the task-d9e845d5c429 acceptance goal tests.

    Every SC-0N case spawns a real built binary, or the real [dune] toolchain, against
    isolated scratch state. This module supplies the scratch-directory and repository-root
    plumbing every case needs; it never fakes a subprocess result. *)

val repo_root : unit -> string
(** [repo_root ()] is the absolute path of the workspace directory that contains
    [dune-project], found by walking up from the current working directory. A Dune build
    copy never carries [dune-project] into [_build], so the walk always lands on the real
    source root even when the caller runs inside a Dune build action. It fails the calling
    test when no such ancestor exists. *)

val fresh_scratch : Eio_unix.Stdenv.base -> root:string -> name:string -> string
(** [fresh_scratch env ~root ~name] creates and returns a fresh, empty, mode-0700
    directory under [root]/.outline/worktree named after [name] and the current process,
    first removing any stale directory of the same name. The caller removes it when done.
*)

val real_path : unit -> string
(** [real_path ()] is the current [PATH] environment value, or ["/usr/bin:/bin"] when
    unset, for building a sanitized child environment that can still locate real tools. *)

val contains : needle:string -> string -> bool
(** [contains ~needle text] is [true] when [needle] occurs in [text]. The empty needle
    always matches. *)
