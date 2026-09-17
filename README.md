# charm

The Charm ecosystem re-derived in pure OCaml. Typed messages, `result`
errors, and Eio fibers replace the Go originals. No CLI flag, environment
variable, or file format is kept for Go compatibility.

## Packages

`charamel.ansi` parses and measures terminal sequences. `charamel.colorprofile`
detects color support from the environment. `charamel.tea` runs Elm-style
terminal apps on a cell grid with per-frame diff. `charamel.lipgloss` styles
text with borders, layout, tables, trees, and lists.
`charamel.harmonica` integrates spring and projectile motion.
`charamel.log` reports through `Logs` in text, logfmt, or JSON.
`charamel.highlight` tokenizes source with data-driven lexers.
`charamel.glamour` renders Markdown through `cmarkit` and `lipgloss`.
`charamel.bubbles` ships reusable TUI components over `charamel.tea`.
`charamel.huh` builds typed forms with accessible fallbacks.
`charamel.cli` runs apps with XDG paths and styled errors.
`charamel.fantasy` streams chat completions from Anthropic, OpenAI-compatible,
OpenAI Responses, and Google providers plus a vendored model catalog.
`charamel-ssh.keygen` generates and reads OpenSSH keys in pure OCaml.
`charamel-ssh.wish` serves TUI apps over SSH through the pure `awa` state
machine.

`gum` prompts for input, choices, and values. `glow` reads and browses
Markdown. `freeze` renders code to SVG and PNG. `sequin` explains escape
sequences. `pop` composes and sends mail. `skate` keeps a local key-value
store. `melt` backs up Ed25519 keys as mnemonic words. `keygen` writes
OpenSSH key pairs. `hotdiva2000` prints memorable names. `crush` is the
agentic coding harness with tools, permissions, MCP, LSP, and a TUI.

## Build and test

```sh
eval $(opam env --switch=rd --set-switch)
opam install awa
opam exec -- dune build --profile release
opam exec -- dune runtest --profile release
opam exec -- dune build @fmt
```

A pre-commit hook runs the same three gates. Enable it with
`git config core.hooksPath .githooks`. The hook calls `opam exec`, which
resolves `dune` through `OPAMSWITCH` rather than `PATH`, so commit from a
shell bootstrapped with `--set-switch` as above. When the hook reports
`Command not found 'dune'`, fix the switch and retry instead of passing
`--no-verify`.

Pure OCaml in this repository: no `foreign_stubs`, no C files.
Dependencies keep whatever they ship (`eio` and `mirage-crypto` carry
their own C). Highlighting uses `re`-based data-driven lexers. Config
files are JSON via `jsont`. Sessions are JSONL. The OCaml floor is
`>= 5.4`. Tests use alcotest.

## Licensing

- This work is Apache-2.0. Read `LICENSE` for the license body and `NOTICE`
  for copyright and third-party attributions.
