#!/usr/bin/env python3
"""Provision the pinned Generalized Notation Notation checkout.

The GNN conformance test (``tests/core/test_gnn_spec.py``) validates this
repository's specification with the *upstream* parser at the revision recorded
in ``.github/gnn-pin.json``. This script materialises that exact revision into a
throwaway checkout with its own interpreter, so the check never silently uses a
sibling working copy.

The layout it produces is the contract the test fixture expects: a Git checkout
whose ``HEAD`` is the pinned revision plus a ``.venv/bin/python`` carrying the
parser's runtime dependencies (``numpy``, ``defusedxml``, ``psutil``).

Idempotent: an existing checkout already at the pinned revision is reused, so
``./run.sh gnn`` is cheap on a second run.
"""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import sys
from pathlib import Path

UPSTREAM = "https://github.com/ActiveInferenceInstitute/Generalized_Notation_Notation.git"
PARSER_DEPENDENCIES = ("numpy", "defusedxml", "psutil")
PIN_PATH = Path(".github/gnn-pin.json")


def run(argv: list[str], **kwargs: object) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(argv, capture_output=True, text=True, check=False, **kwargs)
    if result.returncode != 0:
        sys.stderr.write(f"command failed ({result.returncode}): {' '.join(argv)}\n")
        sys.stderr.write(result.stdout)
        sys.stderr.write(result.stderr)
        raise SystemExit(1)
    return result


def head_of(checkout: Path) -> str:
    result = subprocess.run(
        ["git", "-C", str(checkout), "rev-parse", "HEAD"], capture_output=True, text=True
    )
    return result.stdout.strip()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dest", default=".gate/gnn", help="checkout location (default: .gate/gnn)")
    parser.add_argument("--force", action="store_true", help="re-provision even when the pin matches")
    args = parser.parse_args(argv)

    pin = json.loads(PIN_PATH.read_text(encoding="utf-8"))
    revision = str(pin["revision"])
    tag = str(pin["tag"])
    checkout = Path(args.dest).resolve()

    interpreter = checkout / ".venv" / "bin" / "python"
    if not args.force and (checkout / ".git").exists() and interpreter.is_file():
        if head_of(checkout) == revision:
            print(f"reusing pinned GNN checkout at {checkout} ({revision[:12]})")
            print(checkout)
            return 0

    if checkout.exists():
        shutil.rmtree(checkout)
    checkout.parent.mkdir(parents=True, exist_ok=True)

    run(["git", "init", "--quiet", str(checkout)])
    run(["git", "-C", str(checkout), "remote", "add", "origin", UPSTREAM])
    # A pin that no longer resolves upstream fails here rather than skipping.
    run(["git", "-C", str(checkout), "fetch", "--depth", "1", "origin", revision, f"refs/tags/{tag}:refs/tags/{tag}"])
    run(["git", "-C", str(checkout), "checkout", "--quiet", "--detach", revision])

    run(["uv", "venv", "--python", "3.12", "--no-project", str(checkout / ".venv")])
    run(["uv", "pip", "install", "--python", str(interpreter), *PARSER_DEPENDENCIES])

    print(f"provisioned {UPSTREAM} {tag} ({revision}) at {checkout}")
    print(checkout)
    return 0


if __name__ == "__main__":
    sys.exit(main())
