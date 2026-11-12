#!/usr/bin/env python3
"""CEREBRUM's local gate: one entry point, separately runnable arms.

CI (``.github/workflows/ci.yml``) invokes these arms through ``./run.sh``; no
workflow step re-implements a command that lives here. Each arm is timed and
appended to ``.gate/timings.jsonl`` so ``./run.sh metrics`` can publish the
measured numbers without anyone typing them.

Arms::

    setup        create .venv from uv.lock (dev extras)
    lint         blocking static checks (syntax errors, undefined names)
    types        mypy on the annotated GNN loader surface
    docs-index   regenerate docs/documentation-index.md
    docs         docs index freshness + relative link check
    test-fast    marker-selected quick test pass
    test         full suite with coverage and a JUnit report
    gnn          pinned-upstream GNN conformance check
    debt         measure the non-blocking static-analysis debt
    metrics      render docs/_generated/gate-metrics.md from .gate/ artifacts
    all          lint, types, docs, test, gnn

Usage::

    ./run.sh              # list the arms
    ./run.sh test         # run one arm
    ./run.sh all          # run the whole gate
"""

from __future__ import annotations

import argparse
import json
import os
import platform
import shlex
import subprocess
import sys
import time
from dataclasses import dataclass, field
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
GATE_DIR = ROOT / ".gate"
VENV_PYTHON = ROOT / ".venv" / "bin" / "python"
GNN_CHECKOUT = GATE_DIR / "gnn"

LINT_TARGETS = ("src", "tests", "beyond_cerebrum/src", "scripts")
BLOCKING_FLAKE8_SELECT = "E9,F63,F7,F82"


@dataclass(frozen=True)
class Step:
    argv: tuple[str, ...]
    env: dict[str, str] = field(default_factory=dict)


@dataclass(frozen=True)
class Arm:
    name: str
    help: str
    steps: tuple[Step, ...]
    needs_venv: bool = True
    # `metrics` renders the timing table, so recording its own run would make
    # the freshly written document stale by one row.
    record: bool = True


def py(*args: str) -> tuple[str, ...]:
    return (sys.executable, *args)


ARMS: dict[str, Arm] = {
    "setup": Arm(
        "setup",
        "create .venv from uv.lock with the dev and llm extras (uv must be on PATH)",
        # `llm` is light (aiohttp, openai, python-dotenv, requests) and keeps
        # tests/llm/test_openrouter_contract.py and tests/llm/test_llm_utils.py
        # running instead of module-skipping, which is what the old CI leg did.
        (Step(("uv", "sync", "--locked", "--extra", "dev", "--extra", "llm")),),
        needs_venv=False,
    ),
    "lint": Arm(
        "lint",
        "flake8 syntax/undefined-name errors over src, tests, beyond_cerebrum/src, scripts",
        (
            Step(
                py(
                    "-m",
                    "flake8",
                    f"--select={BLOCKING_FLAKE8_SELECT}",
                    "--max-line-length=100",
                    *LINT_TARGETS,
                )
            ),
        ),
    ),
    "types": Arm(
        "types",
        "mypy on src/core/gnn.py, the annotated GNN loader surface",
        (Step(py("-m", "mypy", "src/core/gnn.py")),),
    ),
    "docs-index": Arm(
        "docs-index",
        "regenerate docs/documentation-index.md from the docs tree",
        (Step(py("scripts/docgen/doc_index.py")),),
    ),
    "docs": Arm(
        "docs",
        "docs index freshness and relative-link check",
        (
            Step(py("scripts/docgen/doc_index.py", "--check")),
            Step(py("scripts/check_links.py")),
        ),
    ),
    "test-fast": Arm(
        "test-fast",
        "quick logic-core pass (core, cases, transformations, utils, beyond_cerebrum)",
        (
            Step(
                py(
                    "-m",
                    "pytest",
                    "-q",
                    "--no-header",
                    "tests/core",
                    "tests/cases",
                    "tests/transformations",
                    "tests/utils",
                    "beyond_cerebrum/src/tests",
                    # The two slowest core files (measured: ~120 s and ~100 s of
                    # the full run) belong to `test`, not to the quick pass.
                    "--deselect",
                    "tests/core/test_neural_network.py",
                    "--deselect",
                    "tests/core/test_active_inference_pomdp.py",
                )
            ),
        ),
    ),
    "test": Arm(
        "test",
        "full suite with coverage and JUnit report (what CI runs)",
        (
            Step(
                py(
                    "-m",
                    "pytest",
                    "-q",
                    "--cov=src",
                    "--cov-report=term-missing",
                    f"--cov-report=json:{GATE_DIR / 'coverage.json'}",
                    f"--junitxml={GATE_DIR / 'junit.xml'}",
                )
            ),
        ),
    ),
    "gnn": Arm(
        "gnn",
        "pinned-upstream GNN conformance (provisions .gate/gnn at the pinned revision)",
        (
            Step(py("scripts/gnn_checkout.py", "--dest", str(GNN_CHECKOUT))),
            Step(
                py("-m", "pytest", "tests/core/test_gnn_spec.py", "-q", "--no-header"),
                env={"GNN_CHECKOUT": str(GNN_CHECKOUT)},
            ),
        ),
    ),
    "debt": Arm(
        "debt",
        "measure non-blocking static-analysis debt into .gate/debt.json",
        (Step(py("scripts/docgen/debt.py")),),
    ),
    "metrics": Arm(
        "metrics",
        "render docs/_generated/gate-metrics.md from the recorded artifacts",
        (Step(py("scripts/docgen/gate_metrics.py")),),
        needs_venv=False,
        record=False,
    ),
    "all": Arm("all", "lint, types, docs, test, gnn", ()),
}

ALL_SEQUENCE = ("lint", "types", "docs", "test", "gnn")


def record_environment() -> None:
    uv = subprocess.run(["uv", "--version"], capture_output=True, text=True, check=False)
    tokens = uv.stdout.split()
    record = {
        "python": platform.python_version(),
        # `uv 0.13.7 (Homebrew ...)` -> record just the version token.
        "uv": tokens[1] if uv.returncode == 0 and len(tokens) > 1 else "unavailable",
        "host": f"{platform.system().lower()}-{platform.machine()}",
    }
    GATE_DIR.mkdir(exist_ok=True)
    (GATE_DIR / "env.json").write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8")


def run_arm(arm: Arm) -> int:
    GATE_DIR.mkdir(exist_ok=True)
    environment = dict(os.environ)
    started = time.perf_counter()
    exit_code = 0
    for step in arm.steps:
        argv = [str(part) for part in step.argv]
        print(f"[{arm.name}] $ {shlex.join(argv)}", flush=True)
        step_env = {**environment, **step.env}
        result = subprocess.run(argv, cwd=ROOT, env=step_env, check=False)
        if result.returncode != 0:
            exit_code = result.returncode
            break
    elapsed = time.perf_counter() - started
    status = "ok" if exit_code == 0 else f"FAILED ({exit_code})"
    if not arm.record:
        # The renderer's own run would make the page it just wrote stale.
        print(f"[{arm.name}] {status} in {elapsed:.1f}s (run not recorded)", flush=True)
        return exit_code
    record = {
        "arm": arm.name,
        # Record `python ...` rather than the absolute interpreter path so the
        # published command is reproducible on another machine.
        "argv": [
            "python" if str(part) == sys.executable else str(part).replace(f"{ROOT}/", "")
            for part in arm.steps[0].argv
        ]
        + (["..."] if len(arm.steps) > 1 else []),
        "seconds": round(elapsed, 2),
        "exit": exit_code,
        "recorded": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
    }
    with (GATE_DIR / "timings.jsonl").open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(record) + "\n")
    print(f"[{arm.name}] {status} in {elapsed:.1f}s", flush=True)
    return exit_code


def list_arms() -> None:
    width = max(len(name) for name in ARMS)
    print("CEREBRUM gate arms (run one with ./run.sh <arm>):\n")
    for name, arm in ARMS.items():
        print(f"  {name.ljust(width)}  {arm.help}")
    print(f"\n  {'all'.ljust(width)}  " + " -> ".join(ALL_SEQUENCE))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("arm", nargs="?", help="arm to run; omit to list them")
    parser.add_argument("rest", nargs=argparse.REMAINDER, help="arguments forwarded to the arm's script")
    args = parser.parse_args(argv)

    if not args.arm:
        list_arms()
        return 0

    name = args.arm
    if name not in ARMS:
        print(f"unknown arm: {name}", file=sys.stderr)
        list_arms()
        return 2

    if name == "all":
        failures: list[tuple[str, int]] = []
        for member in ALL_SEQUENCE:
            exit_code = run_arm(ARMS[member])
            if exit_code != 0:
                failures.append((member, exit_code))
        print()
        if failures:
            for member, exit_code in failures:
                print(f"gate FAILED at {member} (exit {exit_code})")
            return 1
        print("gate OK: " + ", ".join(ALL_SEQUENCE))
        return 0

    arm = ARMS[name]
    if args.rest:
        if len(arm.steps) != 1:
            print(
                f"arm '{name}' has {len(arm.steps)} steps; run its scripts directly "
                "rather than forwarding arguments",
                file=sys.stderr,
            )
            return 2
        arm = Arm(
            arm.name,
            arm.help,
            (Step((*arm.steps[0].argv, *args.rest), arm.steps[0].env),),
            arm.needs_venv,
            arm.record,
        )
    if arm.needs_venv and Path(sys.executable).resolve() != VENV_PYTHON.resolve():
        print(
            f"arm '{name}' needs {VENV_PYTHON}; run ./run.sh setup first",
            file=sys.stderr,
        )
        return 2
    record_environment()
    return run_arm(arm)


if __name__ == "__main__":
    sys.exit(main())
