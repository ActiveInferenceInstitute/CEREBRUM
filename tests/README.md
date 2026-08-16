# Tests

This directory contains the test suite for CEREBRUM, organized into subfolders that mirror `src/` modules.

Run `uv run python -m pytest` for the current suite count and result. Live-API tests are opt-in via `@pytest.mark.live`.

## Quick Start

```bash
# Run the full suite (from the repository root, inside the uv virtualenv)
uv run python -m pytest

# Run a specific module's tests
uv run python -m pytest tests/core/ -v
uv run python -m pytest tests/transformations/ -v
uv run python -m pytest tests/utils/ -v

# Run the suite via the project's test runner (coverage, HTML reports)
python src/scripts/run_tests.py --coverage --html
```

Note: use `uv run python -m pytest` rather than a bare `pytest` so the correct
virtual environment and configuration (see `pytest.ini`) are always used.

## Module Mapping

| Test Directory | Source Module | Tests |
| -------------- | ------------ | ----- |
| `core/` | `src/core/` | Model, ActiveInference, NeuralNetwork, Registry |
| `cases/` | `src/cases/` | CaseManager, All 8 case handlers (parametrized) |
| `transformations/` | `src/transformations/` | Case & animal transformations |
| `models/` | `src/models/` | InsectModel, LinearRegression |
| `utils/` | `src/utils/` | DataGenerator, ArrayUtils, PathUtils, Visualizer |
| `analysis/` | `src/analysis/` | SimulationEffectivenessAnalyzer |
| `llm/` | `src/llm/` | Ollama, OpenRouter |
| `examples/` | `src/examples/` | Environment |
| `visualization/` | `src/visualization/` | Case visualization, transitions, ecosystem |
| `integration/` | Cross-module | Model interactions |
| `pomdp/` | POMDP infrastructure | Per-case POMDP tests |
| `lexicon/` | `src/lexicon/` | Lexicon engine, NLP, declension, graph |

## Detailed Documentation

- **`README_TESTING.md`** — TDD methodology, testing tools, and the linear-regression case tests
- **`AGENTS.md`** — Full directory tree and agent context

## Test Reports

When run through `src/scripts/run_tests.py`, timestamped reports are written to
`tests/output/` (HTML coverage report, JUnit XML, logs, and a summary), with
visualizations under `tests/output/linear_regression/` organized by case.
