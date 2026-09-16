---
title: Shell regex escaping through the bash tool is unstable across calls
date: 2026-09-16
category: workflow-issues
module: .omp harness
problem_type: workflow_issue
component: tooling
severity: medium
tags:
  - "bash-tool"
  - "escaping"
  - "grep"
  - "sed"
---

## Context

Regex-heavy commands sent through this harness's bash tool have failed
silently twice in one session, in different directions: a `grep` pattern
mangled by layer-to-layer unescaping reported 250 and 383 phantom doc-comment
counts for a file that actually contains zero (`grep -cF` proved the truth),
and a `sed -i 's/\bstarts_with ~prefix:/.../g` matched nothing, exited zero,
and left the tree with `Error: Unbound value starts_with` at the next build.
Both failures were silent; the first inflated a campaign's scope estimate by
roughly 3,600 sites before anyone noticed, and the second broke the build.

## Guidance

Use the edit tool with `replace_all: true` for rewrites. Never use `sed -i`
through this tool. Use `grep -cF` with a fixed string for counts of literal
patterns, and treat any count that would change a scope decision as suspect
until re-measured with `-F`. When a mechanical command's output drives a
conclusion, re-verify the load-bearing number a second way.

## Why this matters

The failure mode is silent success: the command exits zero and prints
plausible output, so nothing flags the corruption except a contradictory
re-check. Scope estimates, campaign plans, and build health have all been
wrong in one session from this single cause.

## When to apply

Any grep, sed, or awk whose pattern contains backslashes, quotes, or balanced
delimiters, run through this harness's bash tool. The rule is harness-specific;
in a verified interactive shell the same commands behave normally.
