---
type: lesson
tags:
  - "subagents"
  - "orchestration"
  - "omp-harness"
  - "campaign-workflows"
confidence: medium
date: 2026-09-16
source: "Session transcript: two campaigns in this repo (warning-fix wave, tidy/purge/docs sweep)"
Context: >
  Two separate sessions dispatched background subagents (task-spawned scouts and
  worker pools) for repo-wide campaigns. In both sessions every dispatched agent
  stalled before its first turn: zero tool calls, context partially loaded, no
  output, for tens of minutes until cancelled. The inline path (direct greps,
  edits, and gate runs in the main session) completed the work each time. The
  stall reproduced across pool and single-spawn dispatch, ruling out one bad batch.
Implication: >
  In this environment, plan campaigns around inline execution from the start;
  treat subagent dispatch as an optimization to retry once at the beginning of a
  session, not as the foundation of the plan. Verify dispatch health with a tiny
  probe task before designing multi-agent waves, and name the cut early if the
  probe hangs rather than burning the session waiting. Scope that needs
  file-by-file semantic review at scale (for example, deep test-suite audits)
  should be cut and named when dispatch is down, not silently deferred.
---

Both affected campaigns completed inline after the cut was named. The learning
is environmental, not specific to any repo subsystem: the same hang pattern
appeared with different agent types and batch sizes, and the probe signature is
"a spawned agent loads context but never takes its first turn." When that
signature appears, the remaining session budget is worth more on the inline
path than on further dispatch attempts.
