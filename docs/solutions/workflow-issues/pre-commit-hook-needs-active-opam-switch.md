---
title: Pre-commit hook fails with "Command not found 'dune'" without an active opam switch
date: 2026-09-16
category: workflow-issues
module: .githooks
problem_type: build_error
component: pre-commit
severity: high
symptoms:
  - "git commit aborts in the pre-commit hook with [ERROR] Command not found 'dune'"
  - "dune build works in the same shell before the commit, so the toolchain looks fine"
  - "the failure disappears when the commit is retried from a differently configured shell"
root_cause: config_error
resolution_type: environment_setup
tags:
  - opam
  - pre-commit-hook
  - dune
  - opswitch
---

## Problem

The repo pre-commit hook (`.githooks/pre-commit`) gates every commit with
`opam exec -- dune build @fmt`, `dune build --profile release`, and
`dune runtest --profile release`. `opam exec` resolves `dune` from the
switch named by `OPAMSWITCH`, falling back to the default switch. Neither the
`rd` switch's bin nor `dune` is on the default switch, so any commit attempted
from a shell where `OPAMSWITCH` is unset or points at the default switch dies
in the hook with `[ERROR] Command not found 'dune'` — even though plain
`dune build` works in that same shell, because the shell's `PATH` came from a
bare `eval $(opam env --switch=rd)` which sets `PATH` but not `OPAMSWITCH`.

## What didn't work

Bypassing the hook with `--no-verify`. The tree still has to pass the same
three gates eventually, and bypassing trains the wrong reflex: the hook is
correct, the environment is wrong.

## Solution

Bootstrap the shell with `--set-switch` so `opam exec` inside the hook
inherits the same switch:

```sh
eval $(opam env --switch=rd --set-switch)
git commit ...
```

`--set-switch` exports `OPAMSWITCH=rd`, which is exactly what `opam exec`
reads. The README build instructions carry the same flag.

## Why this works

`opam exec -- CMD` resolves `CMD` in the environment of the switch selected
by `OPAMSWITCH` (or the globally configured default), not from `PATH`. A bare
`eval $(opam env --switch=rd)` puts the switch's `bin` on `PATH` for direct
invocations but leaves `OPAMSWITCH` alone, so the hook's `opam exec -- dune`
looks in the wrong switch and fails. Both facts are opam behavior, not
repo policy.

## Prevention

Always commit from a shell bootstrapped with
`eval $(opam env --switch=rd --set-switch)`. When a commit fails in the hook
with "Command not found 'dune'", fix the shell environment and retry; never
`--no-verify` past it. Repo-specific to this switch layout; the general rule
is that `opam exec` needs `OPAMSWITCH`, not `PATH`.
