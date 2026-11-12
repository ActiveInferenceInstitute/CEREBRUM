# CEREBRUM Repository Context

## Overview

This repository contains the CEREBRUM (Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling) framework. It is designed to integrate symbolic case-based reasoning with Bayesian probabilistic inference.

## Key Directories

- **`src/`**: The core source code for the framework. This is where the primary logic resides.
- **`tests/`**: Automated test suite; run `uv run python -m pytest` for the current count, covering core, cases, models, transformations, utils, and visualization.
- **`models/`**: Generalized Notation Notation specifications for the generative models, with the loader in `src/core/gnn.py` and the drift guard in `tests/core/test_gnn_spec.py`; see `models/README.md` for the pinned upstream revision.
- **`paper/`**: Contains resources for generating the academic paper associated with this project, including LaTeX components, figures, and build scripts.
- **`docs/`**: Documentation for the project, including guides, specifications, and educational materials.
- **`scripts/`**: One-off or orchestration scripts that may not fit into the core library.
- **`beyond_cerebrum/`**: Exploratory or external components related to extending the core framework.

## Environment Setup

This project uses **[uv](https://docs.astral.sh/uv/)** for Python dependency management. Key commands:

```bash
uv venv                         # Create virtual environment
source .venv/bin/activate       # Activate
uv pip install -e .             # Install base dependencies
uv pip install -e ".[dev]"      # Install dev dependencies
uv pip install -e ".[all]"      # Install all optional dependencies
```

## Configuration & Quality Assurance

- **`pyproject.toml`**: The central configuration for the project, defining dependencies, build system, and tool settings.
- **`.pre-commit-config.yaml`**: Configuration for pre-commit hooks.
- **`.coveragerc`**: Code coverage settings.
- **`pytest.ini`**: Pytest runner configuration.

## Development Workflow

- Refer to `CONTRIBUTING.md` for detailed guidelines on how to contribute.
- Use `TODO.md` to track or view the status of ongoing major tasks.

## Documentation Entry Points

- **Framework concepts (cold start):** `docs/README.md` — core spec, how-it-works,
  getting started, case system, and per-audience contributing guides.
- **The gate:** `docs/RUN_GUIDE.md` and `./run.sh` — one entry point with
  separately runnable arms (lint, types, docs, test, test-fast, gnn). CI runs
  the same arms; there is no second copy of a command. Run `./run.sh all` before
  landing a change. Generated pages never hold hand-typed numbers:
  `docs/documentation-index.md` (`./run.sh docs-index`) and
  `docs/_generated/gate-metrics.md` (`./run.sh metrics`).
- **Core mechanics:** `docs/cerebrum_core_spec.md` (compliance spec) and
  `docs/how_it_works.md` (architecture overview); architecture diagram source at
  `docs/diagrams/project_architecture.mermaid`.
- **Generative model specification:** `models/README.md` (GNN source of truth,
  loader, drift guard, upstream pin) and `.github/GNN.md` (the conformance job,
  its display name `gnn conformance (pinned)`, and the `.github/gnn-pin.json`
  revision it parses).
- **Language case mappings:** `docs/language_implementations.md` and `docs/languages/`.
- **Agent orientation:** this file (`AGENTS.md`) plus `CLAUDE.md` (commands and
  architecture notes). Read `docs/README.md` before extending the conceptual surface.
