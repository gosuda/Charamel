(** SC-01 goal harness case.

    [run ()] runs the real release build, [dune build --profile release], at the real
    repository root inside its own owned build directory under [.outline/worktree] with a
    sanitized child environment, so it never reuses or locks the ambient build directory.
    It fails when the build exits non-zero or leaves an expected package binary unbuilt.
*)

val run : unit -> unit
