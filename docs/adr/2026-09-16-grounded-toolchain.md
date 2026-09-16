# Grounded toolchain, 2026-09-16

Every pin below was checked at its own release channel on 2026-09-16.
Repo floors override release-channel picks where noted.

## Pinned set

| Choice | Pinned | Release date | End of support | Channel | Decision |
|---|---|---|---|---|---|
| OCaml (floor) | >= 5.4 | 2026-02-17 (5.4.1) | n/a (per-release) | [ocaml.org/releases](https://ocaml.org/releases) | Repo floor `(ocaml (>= 5.4))` overrides latest-stable 5.5.0 (2026-06-19). CI matrix tests 5.4.0 and 5.5.1; 5.5-only stdlib is forbidden (see incident below) |
| OCaml (latest stable) | 5.5.0 | 2026-06-19 | n/a | [ocaml.org/releases/5.5.0](https://ocaml.org/releases/5.5.0) | Latest stable; local switch runs 5.5.1 |
| dune | 3.24.2 | 2026-08 | n/a | [github.com/ocaml/dune/releases](https://github.com/ocaml/dune/releases) | Latest stable; matches repo rule floor 3.24+ and local install |
| opam | 2.5.2 | 2026-07-09 | n/a | [opam.ocaml.org](https://opam.ocaml.org/) | Latest stable. Pre-release 2.6.0~rc1 (2026-09-08) dropped; setup-ocaml already fetches 2.5.2 |
| ocamlformat | 0.29.0 | n/a | n/a | [github.com/ocaml-ppx/ocamlformat/releases](https://github.com/ocaml-ppx/ocamlformat/releases) | Latest stable; identical pin in `.ocamlformat` and CI install line |
| Alcotest | 1.9.1 (:with-test) | n/a | n/a | [opam.ocaml.org/packages/alcotest](https://opam.ocaml.org/packages/alcotest/) | Latest; matches local install |
| QCheck | not depended | n/a | n/a | [opam.ocaml.org/packages/qcheck-core](https://opam.ocaml.org/packages/qcheck-core/) | Gap: the project rule says "Alcotest plus QCheck" but no dune-project package declares it. Latest is 0.91. No package uses it, so nothing to pin; add only with a real consumer |
| Platform | ubuntu-latest, macos-latest | rolling | rolling | GitHub runner images | Moving-tag baseline; no pin available |

## Incidents this grounding explains

- `bin/pop/mime.ml` used `Option.exists`, which exists only in OCaml 5.5,
  against the declared 5.4 floor. CI 5.4.0 legs failed with
  `Unbound value Option.exists` while the local 5.5.1 switch passed.
  Fixed in `a192c5e` by replacing it with a plain `match`.
  Rule: stdlib additions newer than the declared floor are compile
  failures on CI legs the local switch cannot see.

## Recommended follow-ups

- CI matrix leg `5.4.0` -> `5.4.1`: 5.4.1 (2026-02-17) carries Marshal
  security hardening OSEC-2026-01. Recorded here per ground-latest
  contract; not applied by that pass.
