#!/usr/bin/env python3
"""Provision the pinned Generalized Notation Notation checkout.

The GNN gate validates this repository's specification with the parser at the
revision recorded in ``.github/gnn-pin.json``. Until a release ships the shared
``gnn.importers`` reader, this script owns the local pin/pointer drift check.
It materialises the exact revision into a throwaway checkout with its own
interpreter, so the check never silently uses a sibling working copy.

The layout it produces is the contract the gate expects: a Git checkout whose
``HEAD`` is the pinned revision plus a ``.venv/bin/python`` carrying the
parser's runtime dependencies (``numpy``, ``defusedxml``, ``psutil``).

Idempotent: an existing checkout already at the pinned revision is reused, so
``./run.sh gnn`` is cheap on a second run.
"""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any

UPSTREAM = "https://github.com/ActiveInferenceInstitute/Generalized_Notation_Notation.git"
PARSER_DEPENDENCIES = ("numpy", "defusedxml", "psutil")
ROOT = Path(__file__).resolve().parents[1]
PIN_PATH = Path(".github/gnn-pin.json")
POINTER_PATH = Path(".github/GNN.md")


def load_pin(repo_root: Path = ROOT) -> dict[str, Any]:
    """Read CEREBRUM's pin and reject drift in its conformance pointer."""
    try:
        pin = json.loads((repo_root / PIN_PATH).read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise ValueError(f"cannot read {PIN_PATH}: {exc}") from exc
    if not isinstance(pin, dict) or pin.get("schema") != "gnn-importer-pin/v1":
        raise ValueError(f"{PIN_PATH}: expected schema gnn-importer-pin/v1")
    revision = pin.get("revision")
    if not isinstance(revision, str) or not re.fullmatch(r"[0-9a-f]{40}", revision):
        raise ValueError(f"{PIN_PATH}: revision must be a full lowercase Git commit")
    for key in ("tag", "version"):
        if not isinstance(pin.get(key), str) or not pin[key]:
            raise ValueError(f"{PIN_PATH}: missing {key}")
    specs = pin.get("spec_files")
    if not isinstance(specs, list) or not specs:
        raise ValueError(f"{PIN_PATH}: spec_files must name the specifications")
    for spec in specs:
        if not isinstance(spec, str) or not (repo_root / spec).is_file():
            raise ValueError(f"{PIN_PATH}: declared specification is missing: {spec!r}")
    try:
        pointer = (repo_root / POINTER_PATH).read_text(encoding="utf-8")
    except OSError as exc:
        raise ValueError(f"cannot read {POINTER_PATH}: {exc}") from exc
    for fact in (str(PIN_PATH), "ci.yml", "gnn-conformance"):
        if fact not in pointer:
            raise ValueError(f"{POINTER_PATH}: does not name {fact}")
    if revision not in pointer and revision[:12] not in pointer:
        raise ValueError(f"{POINTER_PATH}: does not name pinned revision {revision[:12]}")
    return pin


def run(argv: list[str], **kwargs: object) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(
        argv, stdin=subprocess.DEVNULL, capture_output=True, text=True, check=False, **kwargs
    )
    if result.returncode != 0:
        sys.stderr.write(f"command failed ({result.returncode}): {' '.join(argv)}\n")
        sys.stderr.write(result.stdout)
        sys.stderr.write(result.stderr)
        raise SystemExit(1)
    return result


def head_of(checkout: Path) -> str:
    result = subprocess.run(
        ["git", "-C", str(checkout), "rev-parse", "HEAD"],
        stdin=subprocess.DEVNULL, capture_output=True, text=True
    )
    return result.stdout.strip()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dest", default=".gate/gnn", help="checkout location (default: .gate/gnn)")
    parser.add_argument("--force", action="store_true", help="re-provision even when the pin matches")
    parser.add_argument("--check", action="store_true", help="check pin/pointer drift without provisioning")
    args = parser.parse_args(argv)

    try:
        pin = load_pin()
    except ValueError as exc:
        parser.error(str(exc))
    print("GNN pin/pointer drift check OK")
    if args.check:
        return 0
    revision = pin["revision"]
    tag = pin["tag"]
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
