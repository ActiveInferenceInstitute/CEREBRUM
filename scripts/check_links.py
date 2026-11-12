#!/usr/bin/env python3
"""Check relative links in the repository's Markdown files.

The repository has no mkdocs build or markdown-link-check setup, so this script
is the documentation link gate used by ``./run.sh docs``.

Checks performed for every tracked ``*.md`` file:

* every inline link/image target and every reference definition that points
  inside the repository resolves to an existing path;
* a fragment (``#anchor``) on a resolved Markdown file matches a heading slug,
  an explicit HTML anchor ``<a id="...">``/``<a name="...">``, or a heading in
  the same file for same-file fragments.

External URLs (``https://``, ``mailto:`` ...) are not fetched; the gate stays
offline and deterministic.
"""

from __future__ import annotations

import argparse
import html
import os
import re
import subprocess
import sys
import unicodedata
from pathlib import Path
from urllib.parse import unquote, urlsplit

LINK_RE = re.compile(
    r"!?\[(?:[^\[\]]|\[[^\]]*\])*\]\(\s*(<[^>]*>|[^\s)]+)(?:\s+[\"'][^\"']*[\"'])?\s*\)"
)
# Reference definitions, excluding GFM footnotes (`[^1]: ...`), whose "target"
# is the first word of the footnote body rather than a link.
REF_DEF_RE = re.compile(r"^ {0,3}\[(?!\^)[^\]]+\]:\s*(\S+)", re.MULTILINE)
INLINE_CODE_RE = re.compile(r"`[^`\n]*`")
FENCE_RE = re.compile(r"^```.*?^```", re.MULTILINE | re.DOTALL)
# `[a|s](F(s,c))` in TeX math is not a link; strip math before scanning.
DISPLAY_MATH_RE = re.compile(r"\$\$.*?\$\$", re.DOTALL)
INLINE_MATH_RE = re.compile(r"(?<!\\)\$[^$\n]+\$")
HEADING_RE = re.compile(r"^ {0,3}#{1,6}\s+(.*?)\s*#*\s*$", re.MULTILINE)
ANCHOR_RE = re.compile(r"<a\s+(?:id|name)\s*=\s*[\"']([^\"']+)[\"']", re.IGNORECASE)
SCHEME_RE = re.compile(r"^[a-zA-Z][a-zA-Z0-9+.\-]*:")

# Trees whose Markdown is input to, or output of, another tool: their links are
# resolved in a different tree than the one on disk.
#   paper/components/  pandoc fragments that paper/assemble_paper.py copies into
#                      paper/output/ and rewrites; `Figure_N.png` is rendered
#                      there from Figure_N.md, so it does not exist next to the
#                      fragment.
#   paper/output/      build output.
#   tests/output/      artifacts written by the test suite.
SKIPPED_PREFIXES = ("paper/components/", "paper/output/", "tests/output/")


def slugify(text: str) -> str:
    """GitHub-style heading slug: lowercase, drop punctuation, spaces to hyphens."""
    text = re.sub(r"<[^>]+>", "", text)
    text = html.unescape(text).strip().lower()
    text = re.sub(r"`|\*|_|~", "", text)
    out = []
    for char in text:
        if char.isalnum() or char in "-_":
            out.append(char)
        elif char.isspace():
            out.append("-")
    slug = "".join(out)
    # GitHub keeps letters of all scripts but normalises accents.
    return unicodedata.normalize("NFC", slug)


def anchors_of(path: Path) -> set[str]:
    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return set()
    found: set[str] = set()
    for match in HEADING_RE.finditer(text):
        base = slugify(match.group(1))
        found.add(base)
        # Duplicate headings get -1, -2 ... suffixes on GitHub; add them all.
        for index in range(1, 6):
            found.add(f"{base}-{index}")
    found.update(ANCHOR_RE.findall(text))
    return found


def iter_links(text: str) -> list[str]:
    text = FENCE_RE.sub("", text)
    text = DISPLAY_MATH_RE.sub("", text)
    text = INLINE_MATH_RE.sub("", text)
    text = INLINE_CODE_RE.sub("", text)
    targets = [match.group(1) for match in LINK_RE.finditer(text)]
    targets.extend(match.group(1) for match in REF_DEF_RE.finditer(text))
    return [target.strip("<>") for target in targets]


def tracked_markdown(root: Path) -> list[Path]:
    # Tracked plus new-but-not-ignored files: a document should be link-checked
    # before it is committed, not only after.
    result = subprocess.run(
        ["git", "-C", str(root), "ls-files", "-z", "--cached", "--others", "--exclude-standard", "--", "*.md"],
        capture_output=True,
        check=False,
    )
    if result.returncode == 0 and result.stdout:
        names = [name for name in result.stdout.decode().split("\0") if name]
        return sorted(root / name for name in names)
    return sorted(p for p in root.rglob("*.md") if ".git" not in p.parts)


def check(root: Path) -> tuple[list[str], list[str], int]:
    """Return (missing path problems, missing anchor problems, skipped file count)."""
    missing: list[str] = []
    bad_anchor: list[str] = []
    anchor_cache: dict[Path, set[str]] = {}
    skipped = 0

    def anchors(path: Path) -> set[str]:
        if path not in anchor_cache:
            anchor_cache[path] = anchors_of(path)
        return anchor_cache[path]

    for source in tracked_markdown(root):
        relative_posix = source.relative_to(root).as_posix()
        if relative_posix.startswith(SKIPPED_PREFIXES):
            skipped += 1
            continue
        try:
            text = source.read_text(encoding="utf-8", errors="replace")
        except OSError as error:  # pragma: no cover - unreadable tracked file
            missing.append(f"{source.relative_to(root)}: unreadable ({error})")
            continue
        for target in iter_links(text):
            if not target or target.startswith("#") and len(target) == 1:
                continue
            if SCHEME_RE.match(target) or target.startswith("//"):
                continue
            split = urlsplit(target)
            if split.scheme or split.netloc:
                continue
            relative = source.relative_to(root).as_posix()
            raw_path = unquote(split.path)
            if raw_path.startswith("/"):
                # A leading slash is site-absolute on GitHub (github.com/docs/...),
                # never an in-repository link; report it instead of guessing.
                missing.append(
                    f"{relative}: {target} -> site-absolute path, "
                    "use a path relative to this file"
                )
                continue
            if raw_path:
                resolved = (source.parent / raw_path).resolve()
                if not resolved.exists():
                    shown = (
                        resolved.relative_to(root)
                        if resolved.is_relative_to(root)
                        else resolved
                    )
                    missing.append(f"{relative}: {target} -> {shown}")
                    continue
                if split.fragment and resolved.is_file() and resolved.suffix == ".md":
                    fragment = slugify(split.fragment)
                    if fragment not in anchors_of(resolved) and split.fragment not in anchors(
                        resolved
                    ):
                        bad_anchor.append(f"{relative}: {target}")
                continue
            fragment = slugify(split.fragment)
            if split.fragment and fragment not in anchors(source) and split.fragment not in anchors(
                source
            ):
                bad_anchor.append(f"{relative}: {target}")
    return missing, bad_anchor, skipped


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=str(Path(__file__).resolve().parents[1]))
    parser.add_argument(
        "--skip-anchors",
        action="store_true",
        help="check only that link targets exist, not that fragments match a heading",
    )
    args = parser.parse_args(argv)
    root = Path(args.root).resolve()

    missing, bad_anchor, skipped = check(root)
    files = len(tracked_markdown(root)) - skipped
    if missing:
        print(f"{len(missing)} broken relative link target(s):")
        for line in missing:
            print(f"  {line}")
    if bad_anchor and not args.skip_anchors:
        print(f"{len(bad_anchor)} unresolved fragment(s):")
        for line in bad_anchor:
            print(f"  {line}")
    failures = len(missing) + (0 if args.skip_anchors else len(bad_anchor))
    scope = (
        f"{files} Markdown files"
        + (f" ({skipped} tool-generated files not checked)" if skipped else "")
    )
    if failures:
        print(f"link check FAILED across {scope} ({failures} problem(s))")
        return 1
    print(f"link check OK: {scope}, no broken relative links")
    return 0


if __name__ == "__main__":
    sys.exit(main())
