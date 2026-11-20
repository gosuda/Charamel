# Charamel

[![build](https://github.com/gosuda/Charamel/actions/workflows/build.yml/badge.svg)](https://github.com/gosuda/Charamel/actions/workflows/build.yml)
[![license](https://img.shields.io/badge/License-Apache_2.0-blue)](LICENSE)

The Charm terminal ecosystem, re-derived in pure OCaml: sixteen libraries and ten
command-line tools for building terminal software, for OCaml developers writing TUIs
without a Go runtime.

`sequin` (installed below) reads terminal bytes and explains them:

```sh
printf '\033[3A' | sequin
# CSI 3 A  Cursor up 3
```

## Why Charamel

- Pure OCaml: no `.c` or `.h` files and no `foreign_stubs` stanza in the tracked tree.
  Only dependencies such as `lwt`, `ctypes` and `mirage-crypto` carry C.
- A re-derivation, not a binding. Typed messages replace Go interfaces, `result`
  replaces `error`, and Lwt fibers replace goroutines. Flags, environment variables and
  file formats are re-derived for OCaml, so a Charamel tool is not a drop-in replacement
  for its upstream namesake.
- One width model: grapheme-based measurement shared by every renderer, so East Asian
  and combining text align in tables, borders and layout.
- Documented interfaces: each library's `.mli` is its documentation, and
  `dune build @doc` renders it as HTML.
- Tested per unit: every library and tool has its own suite — 30 `dune` test targets
  under `test/`, run by `dune runtest --profile release`.

## Quick start

From a checkout of this repository, on any opam switch with OCaml 5.4 or newer:

```sh
opam install . --deps-only --with-test --with-doc
dune build --profile release
printf '\033[3A' | dune exec sequin
# CSI 3 A  Cursor up 3
```

## Install

The packages are not yet on the main opam repository, so they install from source.
Pinning the repository URL registers every `*.opam` package in it:

```sh
opam pin add --no-action https://github.com/gosuda/Charamel.git
opam install sequin        # or any package name: charamel, glow, gum, ...
```

For the whole workspace, clone and install the local packages:

```sh
git clone https://github.com/gosuda/Charamel.git
cd Charamel
opam install .
```

To depend on a library in your own project, pin the repository as above and add the
library name to your `dune` stanza, for example `(libraries charamel.lipgloss)`.

## Libraries

| Library | What it does |
|---|---|
| `charamel.ansi` | Terminal sequences, incremental decoding, colors, styles, and grapheme-based width |
| `charamel.colorprofile` | Color support detected from the environment, with writers that reduce SGR colors |
| `charamel.tea` | Elm-style terminal apps: typed messages, commands, subscriptions, per-frame diff |
| `charamel.lipgloss` | Immutable styles with borders, layout, tables, trees, lists, and a layered canvas |
| `charamel.bubbles` | Seventeen reusable components over `charamel.tea`, from `textinput` to `viewport` |
| `charamel.huh` | Typed forms with accessible fallbacks |
| `charamel.glamour` | CommonMark rendered to ANSI with typed themes |
| `charamel.highlight` | Byte-preserving syntax highlighting from data-driven lexers |
| `charamel.log` | A styled `Logs` reporter in text, logfmt, or JSON |
| `charamel.harmonica` | Spring and projectile motion advanced by a fixed time step |
| `charamel.os` | Terminal control, processes, pseudo-terminals, paths, signals, and virtual clocks on Linux, macOS, and Windows |
| `charamel.cli` | Application runtime with XDG base directories and styled errors |
| `charamel.net` | HTTP calls with streaming bodies over pure-OCaml TLS, and the SSH transport |
| `charamel.fantasy` | Streaming chat completions for Anthropic, OpenAI-compatible, OpenAI Responses, and Google, with a bundled model catalog |
| `charamel-ssh.keygen` | OpenSSH key pairs and parsing for Ed25519 and NIST P-256, P-384, P-521 |
| `charamel-ssh.wish` | TUI apps served over SSH through the pure `awa` state machine |

Each library's `.mli` is its documentation; `dune build @doc` renders the set locally.

## Tools

| Tool | What it does |
|---|---|
| `gum` | Prompts for input, choices, and values |
| `glow` | Reads and browses Markdown |
| `freeze` | Renders code and terminal output to SVG and PNG |
| `sequin` | Explains terminal escape sequences |
| `pop` | Composes and sends mail |
| `skate` | Keeps a local key-value store |
| `melt` | Backs up Ed25519 keys as mnemonic words and restores them |
| `keygen` | Writes OpenSSH key pairs |
| `hotdiva2000` | Prints memorable names |
| `crush` | An agentic coding harness with tools, permissions, MCP, LSP, and a TUI |

Run any of them through dune, for example `dune exec gum -- --help`, or from
`_build/install/default/bin` after `dune build --profile release`.

## Library use

`charamel.lipgloss` styles and lays out text:

```ocaml
open Charamel_lipgloss

let () =
  Table.v ~headers:[ "Name"; "Value" ] ~rows:[ [ "a"; "1" ]; [ "b"; "2" ] ] ()
  |> Table.render |> print_endline

(* ┌────┬─────┐
   │Name│Value│
   ├────┼─────┤
   │a   │1    │
   │b   │2    │
   └────┴─────┘ *)
```

## Interactive examples

| Program | Shows |
|---|---|
| `bubbletea_examples` | The sixty-three upstream Bubble Tea examples, one module per upstream directory |
| `huh_burger` | An interactive burger-ordering form |
| `wish_counter` | A small counter served over SSH |
| `confetti` | A tiny confetti animation served over SSH |

Each Bubble Tea example has an interactive entry and a scripted smoke. Run one by name,
or list all sixty-three:

```sh
dune exec examples/bubbletea_examples.exe -- simple
dune exec examples/bubbletea_examples.exe -- --list
```

## Development

Three gates, the same three the pre-commit hook runs, from a switch with OCaml 5.4 or
newer:

```sh
dune build @fmt
dune build --profile release
dune runtest --profile release
```

- Hook: `.githooks/pre-commit` runs those gates; enable it with
  `git config core.hooksPath .githooks`. It resolves `dune` through `opam exec`, which
  reads `OPAMSWITCH` rather than `PATH`, so commit from a shell bootstrapped with
  `eval $(opam env --switch=rd --set-switch)`.
- Formatter: `.ocamlformat` pins `ocamlformat` to 0.29.0 with `margin = 90`;
  `dune build @fmt` fails on any other version.
- Warnings: every stanza compiles with `-strict-sequence -strict-formats -short-paths
  -principal -w +a` minus warnings 4, 9, 29, 30, 40-42, 44-46, 48, 58, 66, and 67; the
  release profile adds `-warn-error +a`.
- Layout: `lib/` libraries, `ssh/` the SSH tier, `bin/` tools, `test/` one directory per
  unit, `examples/` runnable programs. The upstream Go sources live in a git-ignored
  `.references/` tree that provenance comments cite.

[AGENTS.md](AGENTS.md) holds the working rules for changes.

## License

Apache-2.0. [LICENSE](LICENSE) carries the license body; [NOTICE](NOTICE) carries the
copyright and the MIT attributions for upstream material transcribed from
charmbracelet and others.
