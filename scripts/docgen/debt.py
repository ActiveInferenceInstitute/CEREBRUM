#!/usr/bin/env python3
"""Measure static-analysis debt and record it for ``docs/_generated/gate-metrics.md``.

The blocking lint arm (``./run.sh lint``) only stops syntax errors and undefined
names. The wider style and typing findings are real but pre-existing, so this
measurement records their current size instead of gating on them: the numbers in
the generated metrics page come from here, never from a hand-typed count.

Writes ``.gate/debt.json`` and always exits 0 (it measures; it does not judge).
"""

from __future__ import annotations

import json
import re
import subprocess
import sys
from pathlib import Path

GATE_DIR = Path(".gate")
TARGETS = ["src"]
TEST_TARGETS = ["src", "tests", "beyond_cerebrum/src"]


def run(argv: list[str]) -> str:
    result = subprocess.run(argv, capture_output=True, text=True, check=False)
    return result.stdout + result.stderr


def main() -> int:
    python = sys.executable
    GATE_DIR.mkdir(exist_ok=True)

    flake8_argv = [
        python, "-m", "flake8", "--max-line-length=100", "--extend-ignore=E203,W503",
        "--max-complexity=10", *TARGETS,
    ]
    flake8_out = run(flake8_argv)
    flake8_count = len([line for line in flake8_out.splitlines() if re.search(r":\d+:\d+: ", line)])

    black_argv = [python, "-m", "black", "--check", *TEST_TARGETS]
    black_out = run(black_argv)
    black_match = re.search(r"(\d+) files? would be reformatted", black_out)
    black_count = int(black_match.group(1)) if black_match else (
        len([line for line in black_out.splitlines() if line.startswith("would reformat")])
    )

    isort_argv = [
        python, "-m", "isort", "--check-only", "--profile", "black", "--line-length", "100",
        *TEST_TARGETS,
    ]
    isort_out = run(isort_argv)
    isort_count = len([line for line in isort_out.splitlines() if line.startswith("ERROR")])

    mypy_argv = [python, "-m", "mypy", *TARGETS]
    mypy_out = run(mypy_argv)
    mypy_match = re.search(r"Found (\d+) errors?", mypy_out)
    mypy_count = int(mypy_match.group(1)) if mypy_match else 0

    record = {
        "flake8": {"command": " ".join(flake8_argv[2:]), "count": flake8_count},
        "black": {"command": " ".join(black_argv[2:]), "count": black_count},
        "isort": {"command": " ".join(isort_argv[2:]), "count": isort_count},
        "mypy": {"command": " ".join(mypy_argv[2:]), "count": mypy_count},
    }
    (GATE_DIR / "debt.json").write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8")
    for name, entry in record.items():
        print(f"{name:8} {entry['count']:>6}  ({entry['command']})")
    print(f"wrote {GATE_DIR / 'debt.json'}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
