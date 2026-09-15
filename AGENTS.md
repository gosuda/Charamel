Per-task goals live in .agent-tasks/task-d9e845d5c429/GOALS.md.

Every module has an `.mli` designed before its `.ml`. The central type is
`t` and stays abstract wherever an invariant must hold. No `Obj.magic`.
Build with strict flags
`(:standard -strict-sequence -strict-formats -short-paths -principal -w +a-4-9-29-30-40..42-44..46-48-50-58-66-67)`
and `-warn-error +a` in the release profile.
