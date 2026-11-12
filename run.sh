#!/usr/bin/env bash
# CEREBRUM's single gate entry point.
#
#   ./run.sh                 list the arms
#   ./run.sh setup           create .venv from uv.lock (dev extras)
#   ./run.sh lint            blocking static checks
#   ./run.sh types           mypy on the annotated GNN loader surface
#   ./run.sh docs            docs index freshness + relative link check
#   ./run.sh test-fast       marker-selected quick pass
#   ./run.sh test            full suite with coverage (what CI runs)
#   ./run.sh gnn             pinned-upstream GNN conformance check
#   ./run.sh debt            measure non-blocking static-analysis debt
#   ./run.sh metrics         rewrite docs/_generated/gate-metrics.md
#   ./run.sh all             lint, types, docs, test, gnn
#
# The arms are defined once, in scripts/gate.py; CI calls this script so no
# workflow step re-implements a command. See docs/RUN_GUIDE.md.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# `setup`, `metrics` and arm listing do not need the project interpreter, so the
# gate can bootstrap a checkout that has no .venv yet.
PYTHON="${CEREBRUM_PYTHON:-$ROOT/.venv/bin/python}"
if [ ! -x "$PYTHON" ]; then
    PYTHON="$(command -v python3 || true)"
fi
if [ -z "$PYTHON" ]; then
    echo "error: no python3 on PATH; install Python 3.13 and rerun" >&2
    exit 2
fi

exec "$PYTHON" "$ROOT/scripts/gate.py" "$@"
