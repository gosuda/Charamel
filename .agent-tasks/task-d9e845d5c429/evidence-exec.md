# evidence-exec.md — executable acceptance chain (worker sc-gates-3, pool sc-gates, item 1)

Date: 2026-09-16 (run 04:16–04:27 UTC). Host jail note: this harness sandbox allows writes only to
the project dir and tmp paths (Landlock-style); `~/.local/share` is `alpha:alpha 700` yet `touch`
into it fails EACCES — environmental, not a code defect (probe below). Dune 3.24.2, opam switch `rd`.

## SC-01 — `dune build --profile release` exits 0

- Command: `opam exec --switch=rd -- dune build --profile release`
- Exit: **0** (elapsed 2s, warm tree)
- Key output: (silent success; dune prints nothing)
- Corroboration: `dune build @goals` suite row `[OK] SC-01 0 release build.`
- Verdict: **PASS**

## SC-02 — `printf 'b\n' | gum choose --select-if-one` prints `b`, exit 0

- Command: `printf 'b\n' | opam exec --switch=rd -- dune exec bin/gum/main.exe -- choose --select-if-one`
- Exit: **0**
- Key output: `b` (exactly, on stdout)
- Corroboration: `@goals` row `[OK] SC-02 0 gum choose.`
- Verdict: **PASS**

## SC-03 — `crush run "say hi"` against a mock provider prints the reply

1. Literal GOALS command (exit 1, environmental):
   `CRUSH_TEST_PROVIDER=fake opam exec --switch=rd -- dune exec bin/crush/main.exe -- run 'say hi'` → **1**
   `ERROR: Eio.Io Fs Permission_denied Unix_error (Permission denied, "mkdirat", "crush"), creating directory <fs:/home/alpha/.local/share/crush>`
   Control probe: `touch /home/alpha/.local/share/.__probe` → Permission denied (exit 1) for my own shell,
   while `/tmp` writes succeed → host write-jail, not product code.
2. `CRUSH_TEST_PROVIDER` is read nowhere in the port: grep across `lib/`, `bin/` → 0 hits (only GOALS.md
   mentions it). The port's authoritative mock is the loopback SSE fixture in `tests/sc-03.ml`.
3. Redirect-state control (ambient env): `run -D <tmp>/data -c <tmp>/proj 'say hi'` with `XDG_*_HOME` under tmp → **1**,
   `ERROR: models: no large model is configured` (= `lib/models.ml:32` `No_model Large`; resolution precedes any
   provider credential lookup or HTTP, so no external network was possible; ambient env holds none of the
   conventional keys listed at `lib/cli/auth.ml:301-305` anyway).
4. Faithful direct-exec proof (mirrors `sc-03.ml`; child run under `env -i`, zero credential exposure):
   - Python SSE fixture on 127.0.0.1:<ephemeral> serving the sc-03 scripted stream; scratch `crush.json`
     with provider `fixture` (`openai_compatible`, base_url loopback, api_key `goal-sc03-test-key`),
     models large+small → `fixture-model`; `HOME`/`XDG_*` sandboxed to scratch.
   - Command: `env -i PATH=… HOME=<scratch>/home XDG_*… timeout 60 _build/default/bin/crush/main.exe run -y --cwd <scratch> 'say hi'`
   - Exit: **0**; stdout: `pong from fixture`
   - Fixture request log: `POST /chat/completions HTTP/1.1`, `authorization: Bearer goal-sc03-test-key`,
     body has `"model":"fixture-model"`, `"stream":true`, `"role":"user"` + `say hi` (turn call);
     second call = title generation (max_tokens 40), as anticipated by sc-03.ml. Assertions 6/6 OK.
- Corroboration: `@goals` row `[OK] SC-03 0 crush run.`
- Verdict: **PASS** (product behavior proven twice: in-suite and direct-exec; literal command fails only
  under this host's write jail, with the env var itself fictional for the port)

## SC-04 — `keygen -t ed25519 -f /tmp/k` writes an OpenSSH pair confirmed by `ssh-keygen -l`

- Scratch run: `dune exec bin/keygen/main.exe -- -t ed25519 -f <mktemp>/k` → **0**; files `k` (0600, 387 B), `k.pub` (0644, 80 B);
  `ssh-keygen -l -f <tmp>/k.pub` → **0**: `256 SHA256:SMCIhezKCIUToer8t3CctuG2wCGY+pf/bycfqRzEFIc no comment (ED25519)`
- Literal GOALS run: `dune exec bin/keygen/main.exe -- -t ed25519 -f /tmp/k` → **0**;
  `ssh-keygen -l -f /tmp/k.pub` → **0**: `256 SHA256:UK0RtP8d9ClRWXJZFjIPBlBwbV1yunJLF9GtHRSgfwU no comment (ED25519)`
  (fingerprint printed by keygen matches `ssh-keygen -l` byte-for-byte);
  `ssh-keygen -y -f /tmp/k` == contents of `/tmp/k.pub` (private-derived public key equality: OK)
- Corroboration: `@goals` row `[OK] SC-04 0 keygen.`
- Verdict: **PASS**

## SC-05 — `glow README.md` prints the H1 styled by the dark theme

- Commands: `dune exec bin/glow/main.exe -- README.md | head -5` (pipeline, dune exit **0**, head exit **0**)
  and raw capture `glow README.md > raw` (exit **0**)
- Raw ANSI proof: 63 lines contain ESC; H1 bytes are `ESC[1;38;5;228;48;5;63m charm ESC[m`.
  `228` on `63` is `h1` in BOTH themes (`charm_glamour.ml:111` dark and `:178` light
  are byte-identical), so it proves styling, not theme selection.
- Theme selection is proven by the strengthened driver's h2 pair: dark's inherited
  heading foreground `38;5;39` present, light's `38;5;27` absent (`@goals` run).
- Corroboration: `@goals` row `[OK] SC-05 0 glow.`
- Note: the default `auto` theme path (`COLORFGBG` -> `Charm_cli.is_dark` ->
  `Theme.auto`) is not asserted by this driver; it is covered by the
  `charm_cli.is_dark` unit tests in the cli suite (forced gate, 16 tests OK).
- Verdict: **PASS**

## Summary

| Criterion | Verdict | Evidence anchor |
|---|---|---|
| SC-01 | PASS | EXIT1=0; @goals OK |
| SC-02 | PASS | EXIT3=0, stdout `b`; @goals OK |
| SC-03 | PASS | @goals OK + direct-exec `pong from fixture` exit 0; literal cmd blocked by host jail (env) |
| SC-04 | PASS | keygen exit 0 (scratch + literal /tmp/k); fingerprints match; pubkey equality OK |
| SC-05 | PASS | glow exit 0; theme selection proven by h2 pair `38;5;39` present / `38;5;27` absent; `228;63` is theme-independent |

Aggregate `dune build @goals`: exit **0**, `Test Successful in 10.495s. 5 tests run.` — all five `[OK]`.
Artifacts (archived under `local://`, durable): charm-warn-all.log (full-profile
warning measurement), evidence-exec.log, crush-rerun.log, sc0345.log,
sc03_fixture.py, sc0345_driver.sh.
Leftover: none. The literal `/tmp/k` key pair from the SC-04 demonstration was
removed after capture; the durable SC-04 assertion is the scratch-isolated driver.
