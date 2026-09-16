# task-d9e845d5c429

## Goal
Port all the charm ecosystems to Ocaml, but not just naive ports; Make it better.

## Success criteria
- [ ] SC-01: `dune build --profile release` exits 0 with every package
- [ ] SC-02: `printf 'b\n' | gum choose --select-if-one` prints `b` and exits 0
- [ ] SC-03: `crush run "say hi"` against a mock provider prints the reply
- [ ] SC-04: `keygen -t ed25519 -f /tmp/k` writes an OpenSSH key pair whose fingerprint `ssh-keygen -l` confirms
- [ ] SC-05: `glow README.md` prints the H1 styled by the dark theme

## Verification
- SC-01: `.agent-tasks/task-d9e845d5c429/tests/sc-01.ml`, `dune build --profile release`
- SC-02: `.agent-tasks/task-d9e845d5c429/tests/sc-02.ml`, `printf 'b\n' | dune exec bin/gum/main.exe -- choose --select-if-one`
- SC-03: `.agent-tasks/task-d9e845d5c429/tests/sc-03.ml`, `dune build @goals` (the driver runs `crush run "say hi"` against a loopback mock provider)
- SC-04: `.agent-tasks/task-d9e845d5c429/tests/sc-04.ml`, `dune exec bin/keygen/main.exe -- -t ed25519 -f /tmp/k && ssh-keygen -l -f /tmp/k.pub`
- SC-05: `.agent-tasks/task-d9e845d5c429/tests/sc-05.ml`, `dune exec bin/glow/main.exe -- README.md`

## Boundaries
- Project-stable rules remain in project configuration.
- Task-specific goals and criteria remain under `.agent-tasks/task-d9e845d5c429/`.
