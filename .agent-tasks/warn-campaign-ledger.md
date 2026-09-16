# Warning campaign ledger — classes 4/40-44 (2026-09-16)

## Shipped (local main, unpushed)
- `8fc1a57` refactor: qualify ambiguous record labels ahead of warnings 40/42
- `4c217df` refactor: qualify remaining dotted-type record labels
- Both: full gates green (fmt, release build, 169-test suite). ~1,500 sites
  qualified with behavior-preserving module-qualified record labels.

## State
- HEAD = 4c217df. Tree clean, green under OLD flags
  (`+a-4-9-29-30-40..42-44..46-48-58-66-67`, classes 4/40-44 suppressed).
- b3622d2 "Update README.md" (deletes verified toolchain pins) — provenance
  unknown (arrived mid-campaign, matches a skill ping; possibly external).
  Flagged to user; revert if unintended.

## Remaining to enable warnings 4/40-44 (~5,600 warnings, per latest full
## harvest on the checkpointed tree)
- 42-only sites (~3,450): 42 fires even at alias-qualified accesses
  (e.g. `km.Keymap.prev`); needs per-site type annotations on receivers —
  chained receivers (`a.b.c`) cannot take the annotation form (proven:
  `a.(b : T).c` is a syntax error; token-level damage earlier).
- 41 (~1,300): "belongs to several types" — first-listed type is selected;
  qualify with that type's module; bare type names need the type index.
- 44 (~340): open-shadowing (`Eio.Path.(...)` shadows `/`) — per-site open
  scoping or operator qualification.
- 4 (~555): fragile-match — per-site match restructuring; multi-line spans.
- 40 residue (~375): unqualified-type single-field sites (~290, need
  type/field→module index from source), multi-field record blocks (~175
  blocks covering many fields each; wrap whole record in `Mod.( ... )` by
  hand — automated wrap proven unsafe: compiler char ranges split tokens).

## Re-run recipe (fresh session, healthy subagents preferred)
1. Flip flags: in every tracked dune file replace
   `+a-4-9-29-30-40..42-44..46-48-58-66-67` with
   `+a-9-29-30-45-46-48-58-66-67`; root `dune` release env holds
   `-warn-error +a` (temporarily `-a` for harvesting only, restore after).
2. Build ONLY as `eval $(opam env --switch=rd --set-switch) && dune ...`
   (bare PATH = system OCaml 5.4.0 → CMI failures; switch rd = 5.5.1).
3. Harvest with `dune clean && dune build --profile release 2> log`
   (incremental builds undercount; always clean).
4. Fixer: /tmp/fix_warn40.py (dotted-type W40 sites; qualified-label
   insertion form). Script the 41/44/4 classes fresh — no infrastructure
   exists for them.
5. Loop fix→clean-build; land flags + fixes as the final commit only when
   the tree is warning-free under +a.

## Infrastructure notes
- Subagent spawn was hung all session (probe blocked 30s+); two pool waves
  burned ~1h, zero completions. Inline scripting only.
- /tmp/fix_warn40.py parser: W40_RE tightened to require dotted type paths;
  unqualified names silently skipped (go manual/index).
- OVERRIDE map in fixer: (file, ident, tpath) → module, only for aliased
  record types (Tool.diagnostic→Lsp, Keymap.binding→Charm_bubbles.Key_binding).
