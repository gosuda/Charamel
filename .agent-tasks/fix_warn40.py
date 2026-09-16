#!/usr/bin/env python3
"""Apply mechanical fixes for OCaml warnings 40/42 from a dune build log.

Warning 40 [name-out-of-scope]: "<ident> was selected from type <T>."
Warning 42 [disambiguated-name]: same site, forward-compat note.

Fix: qualify the record label at the site with its type path
(`{ left = ..` -> `{ Border.left = ..`). Access sites (`r.left`) are
listed for manual repair instead of being rewritten.

Two files are excluded: their warning-40 paragraphs name a type whose
module does not define the label (re-export), so any derived qualifier
fails to compile. Their sites are fixed by hand.
"""
import re
import sys

EXCLUDE = set()

# Labels of aliased record types: the printed type path names the alias
# module, but the label resolves only through its defining module.
OVERRIDE = {
    ("bin/crush/lib/tools_lsp.ml", "path", "Tool.diagnostic"): "Lsp",
    ("bin/crush/lib/tools_lsp.ml", "line", "Tool.diagnostic"): "Lsp",
    ("bin/crush/lib/tools_lsp.ml", "col", "Tool.diagnostic"): "Lsp",
    ("bin/crush/lib/tools_lsp.ml", "severity", "Tool.diagnostic"): "Lsp",
    ("bin/crush/lib/tools_lsp.ml", "message", "Tool.diagnostic"): "Lsp",
    ("lib/huh/field_impl.ml", "keys", "Keymap.binding"): "Charm_bubbles.Key_binding",
}

BLOCK_RE = re.compile(r'^File "([^"]+)", line (\d+), characters (\d+)-(\d+):')
MULTI_RE = re.compile(r'^File "([^"]+)", lines (\d+)-(\d+), characters (\d+)-(\d+):')
W40_RE = re.compile(r"was selected from type ((?:[A-Za-z_]\w*\.)+[A-Za-z_]\w*)")
W40_MULTI_RE = re.compile(
    r"this record of type ((?:[A-Za-z_]\w*\.)+[A-Za-z_]\w*) contains fields")
ECHO_RE = re.compile(r"^\s*\d+ \||^\s*\^+\s*$|^\s*\.\.\.*")


def parse_log(path):
    """Return dict: (file, line, c1, c2) -> (ident, type_path) from warning 40.

    The log interleaves compiler commands, File headers, source echoes,
    and warning paragraphs that may wrap across lines. Only warning
    paragraph text is collected; echoes and commands are ignored.
    """
    sites = {}
    manual_multi = []
    cur = None
    para = None

    def flush():
        nonlocal para
        if para is not None and cur is not None:
            m = W40_RE.search(para)
            if m:
                ident = para.split("]:", 1)[1].strip().split(" ")[0]
                sites[cur] = (ident, m.group(1))
            else:
                mm = W40_MULTI_RE.search(para)
                if mm:
                    manual_multi.append(cur)
        para = None

    with open(path, encoding="utf-8", errors="replace") as fh:
        for raw in fh:
            line = raw.rstrip("\n")
            m = BLOCK_RE.match(line)
            if m:
                flush()
                cur = (m.group(1), int(m.group(2)), int(m.group(3)), int(m.group(4)))
                continue
            if MULTI_RE.match(line) or line.startswith("File "):
                flush()
                cur = None
                continue
            if line.startswith("Warning "):
                flush()
                para = line
                continue
            if cur is None or para is None:
                continue
            if ECHO_RE.match(line) or not line.strip():
                continue
            para += " " + line.strip()
    flush()
    print(f"parse: {len(sites)} sites, {len(manual_multi)} multi-field records deferred", file=sys.stderr)
    return sites


def module_of(tpath):
    """Derive the defining module path from a printed type path.

    "Border.t" -> "Border", "Eio_unix.Pty.winsize" -> "Eio_unix.Pty".
    Unqualified type names carry no module; caller falls back to manual.
    """
    return tpath.rsplit(".", 1)[0] if "." in tpath else None


def apply_site(lines, fname, lineno, c1, c2, ident, tpath, manual):
    idx = lineno - 1
    if idx >= len(lines):
        manual.append((lineno, "out of range"))
        return False
    line = lines[idx]
    frag = line[c1:c2]
    if frag != ident:
        manual.append((lineno, f"fragment mismatch: {frag!r} != {ident!r}"))
        return False
    before = line[:c1].rstrip()
    mod = OVERRIDE.get((fname, ident, tpath)) or module_of(tpath)
    if before.endswith("."):
        # record field access r.ident or a.b.ident: qualify the label with
        # its defining module between receiver and label.
        if mod is None:
            manual.append((lineno, f"unqualified type {tpath}"))
            return False
        lines[idx] = line[:c1] + mod + "." + line[c1:]
        return True
    if before.endswith("{") or before.endswith(";") or re.search(r"\bwith$", before):
        if mod is None:
            manual.append((lineno, f"unqualified type {tpath}"))
            return False
        lines[idx] = line[:c1] + mod + "." + line[c1:]
        return True
    m = re.search(r"~[a-z_][A-Za-z0-9_']*:$", before)
    if m and mod:
        lines[idx] = line[:c1] + mod + "." + line[c1:]
        return True
    manual.append((lineno, f"context: ...{before[-25:]!r}"))
    return False


def main():
    log_path, target = sys.argv[1], sys.argv[2]
    sites = parse_log(log_path)
    by_file = {}
    for (f, ln, c1, c2), v in sites.items():
        by_file.setdefault(f, []).append((ln, c1, c2, v[0], v[1]))
    files = [target] if target != "ALL" else sorted(by_file)
    total_fix = total_manual = 0
    for f in files:
        if f in EXCLUDE:
            entries = sorted(by_file.get(f, []), key=lambda e: (e[0], e[1]), reverse=True)
            total_manual += len(entries)
            print(f"{f}: excluded (manual)")
            continue
        entries = sorted(by_file.get(f, []), key=lambda e: (e[0], e[1]), reverse=True)
        if not entries:
            continue
        with open(f, encoding="utf-8") as fh:
            lines = fh.readlines()
        fixed = manual = 0
        man_rows = []
        for ln, c1, c2, ident, tpath in entries:
            if apply_site(lines, f, ln, c1, c2, ident, tpath, man_rows):
                fixed += 1
            else:
                manual += 1
        with open(f, "w", encoding="utf-8") as fh:
            fh.writelines(lines)
        total_fix += fixed
        total_manual += manual
        print(f"{f}: fixed={fixed} manual={manual}")
        for row in man_rows:
            print(f"  MANUAL line {row[0]}: {row[1]}")
    print(f"TOTAL fixed={total_fix} manual={total_manual}")


if __name__ == "__main__":
    main()
