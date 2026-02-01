# CEREBRUM Repository Context

## Overview

This repository contains the CEREBRUM (Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling) framework. It is designed to integrate symbolic case-based reasoning with Bayesian probabilistic inference.

## Key Directories

- **`src/`**: The core source code for the framework. This is where the primary logic resides.
- **`paper/`**: Contains resources for generating the academic paper associated with this project, including LaTeX components, figures, and build scripts.
- **`docs/`**: Documentation for the project, including guides, specifications, and educational materials.
- **`tools/`**: Utility scripts and tools for maintenance and development.
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
- Use `PROGRESS_REPORT.md` to track or view the status of ongoing major tasks.
