# charm

The Charm ecosystem re-derived in pure OCaml. Typed messages, `result`
errors, and Eio fibers replace the Go originals. No CLI flag, environment
variable, or file format is kept for Go compatibility.

## Packages

`charm.ansi` parses and measures terminal sequences. `charm.colorprofile`
detects color support from the environment. `charm.tea` runs Elm-style
terminal apps on a cell grid with per-frame diff. `charm.lipgloss` styles
text with borders, layout, tables, trees, and lists.
`charm.harmonica` integrates spring and projectile motion.
`charm.log` reports through `Logs` in text, logfmt, or JSON.
`charm.highlight` tokenizes source with data-driven lexers.
`charm.glamour` renders Markdown through `cmarkit` and `lipgloss`.
`charm.bubbles` ships reusable TUI components over `charm.tea`.
`charm.huh` builds typed forms with accessible fallbacks.
`charm.cli` runs apps with XDG paths and styled errors.
`charm.fantasy` streams chat completions from Anthropic, OpenAI-compatible,
OpenAI Responses, and Google providers plus a vendored model catalog.
`charm-ssh.keygen` generates and reads OpenSSH keys in pure OCaml.
`charm-ssh.wish` serves TUI apps over SSH through the pure `awa` state
machine.

`gum` prompts for input, choices, and values. `glow` reads and browses
Markdown. `freeze` renders code to SVG and PNG. `sequin` explains escape
sequences. `pop` composes and sends mail. `skate` keeps a local key-value
store. `melt` backs up Ed25519 keys as mnemonic words. `keygen` writes
OpenSSH key pairs. `hotdiva2000` prints memorable names. `crush` is the
agentic coding harness with tools, permissions, MCP, LSP, and a TUI.

## Build and test

```sh
eval $(opam env --switch=rd)
opam install awa
opam exec -- dune build --profile release
opam exec -- dune runtest --profile release
opam exec -- dune build @fmt
```

A pre-commit hook runs the same three gates. Enable it with
`git config core.hooksPath .githooks`; bypass once with `--no-verify`.

Pure OCaml in this repository: no `foreign_stubs`, no C files.
Dependencies keep whatever they ship (`eio` and `mirage-crypto` carry
their own C). Highlighting uses `re`-based data-driven lexers. Config
files are JSON via `jsont`. Sessions are JSONL. The OCaml floor is
`>= 5.4`. Tests use alcotest.

Toolchain pins verified 2026-09-15: OCaml 5.5.1 with floor `>= 5.4`,
dune 3.24.2, eio 1.5, cmdliner 2.1.1, fmt 0.11.0, logs 0.10.0,
jsont 0.4.0, cmarkit 0.4.0, uucp 17.0.0, uuseg 17.0.0, uutf 1.0.4,
re 1.14.0, mtime 2.2.0, ptime 1.2.0, tls 2.1.3, cohttp-eio 6.3.0,
x509 1.2.0, ca-certs 1.0.3, mirage-crypto 2.4.1, digestif 1.3.1,
base64 3.5.2, alcotest 1.9.1, awa 0.6.1, ocamlformat 0.29.0.

## Licensing

- This work is Apache-2.0. Read `LICENSE` for the license body and `NOTICE`
for copyright and third-party attributions. 
- The `crush` project under the Functional Source License 1.1 with an MIT
future grant, which stand as prior art rather than inherited terms.
