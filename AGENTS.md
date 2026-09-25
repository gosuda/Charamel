# AGENTS.md

## Gates

- Before declaring work done, bootstrap with `eval $(opam env --switch=rd --set-switch)` and run all three: `dune build @fmt`, `dune build --profile release`, `dune runtest --profile release`. [Tony Hoare]
- When the pre-commit hook reports `Command not found 'dune'`, fix the `rd` opam switch and retry; NEVER bypass a red gate with `git commit --no-verify`. [Tony Hoare]
- Keep every stanza warning-clean under the repo flags (`-strict-sequence -strict-formats -short-paths -principal`, release adds `-warn-error +a`). [Tony Hoare]
- Format with the pinned `ocamlformat 0.29.0` (`margin = 90`); NEVER upgrade or work around the pin. [Otl Aicher]

## Purity

- NEVER add `.c` or `.h` files, `foreign_stubs` stanzas, a Go runtime, CGo, or flags, environment variables, or file formats kept for Go compatibility: Charamel is a re-derivation, not a drop-in replacement. [J.R.R. Tolkien]

## Interfaces

- Document each library's public surface in its `.mli`; the `.mli` is the documentation. [Tony Hoare]

## Provenance

- Keep upstream originals inside the git-ignored `data_only_dirs` `.references/` tree; NEVER promote them into `lib/`, `ssh/`, `bin/`, or `test/`. [Christopher Alexander]
- Preserve the third-party attributions in `NOTICE` when transcribing upstream material. [Richard Feynman]
