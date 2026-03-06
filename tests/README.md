# Tests

This directory contains the test suite for CEREBRUM, organized into subfolders that mirror `src/` modules.

## Quick Start

```bash
# Run all 253 tests
python -m pytest tests/ -v

# Run a specific module's tests
python -m pytest tests/core/ -v
python -m pytest tests/transformations/ -v
python -m pytest tests/utils/ -v
```

## Module Mapping

| Test Directory | Source Module | Tests |
| -------------- | ------------ | ----- |
| `core/` | `src/core/` | Model, ActiveInference, NeuralNetwork, Registry |
| `cases/` | `src/cases/` | CaseManager |
| `transformations/` | `src/transformations/` | Case & animal transformations |
| `models/` | `src/models/` | InsectModel, LinearRegression |
| `utils/` | `src/utils/` | DataGenerator, ArrayUtils, PathUtils |
| `analysis/` | `src/analysis/` | SimulationEffectivenessAnalyzer |
| `llm/` | `src/llm/` | Ollama, OpenRouter |
| `examples/` | `src/examples/` | Environment |
| `integration/` | Cross-module | Model interactions |
| `pomdp/` | POMDP infrastructure | Per-case POMDP tests |

## Detailed Documentation

- **`README_TESTING.md`** — TDD methodology and linear regression test guide
- **`Testing_README.md`** — Legacy testing context
- **`AGENTS.md`** — Full directory tree and agent context
