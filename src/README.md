# Source Code

This directory contains the Python source code for CEREBRUM.

## Quick Start

```python
# Core framework
from src.core import Model, Case, ActiveInferenceModel, NeuralNetworkModel

# Case implementations
from src.cases import NominativeCase, AccusativeCase, CaseManager

# Transformations
from src.transformations import transform_case, create_case_relationship

# Utilities
from src.utils import DataGenerator, Visualizer

# Analysis
from src.analysis import SimulationEffectivenessAnalyzer
```

## Directory Structure

| Directory          | Purpose                      | Key Exports                                                                      |
| ------------------ | ---------------------------- | -------------------------------------------------------------------------------- |
| `core/`            | Foundation classes           | `Model`, `Case`, `ActiveInferenceModel`, `NeuralNetworkModel`, `ModelRegistry`   |
| `cases/`           | Case implementations         | `NominativeCase`, `AccusativeCase`, `DativeCase`, etc., `CaseManager`            |
| `models/`          | Cognitive models             | `Model`, `Case`, `CaseDefinitions`, `LinearRegressionModel`                      |
| `transformations/` | Case transformation logic    | `transform_case`, `revert_case`, `create_case_relationship`                      |
| `utils/`           | Utilities and visualization  | `DataGenerator`, `Visualizer`, animation functions                               |
| `analysis/`        | Simulation assessment        | `SimulationEffectivenessAnalyzer`                                                |
| `lexicon/`         | NLP and linguistic analysis  | `LexiconEngine`, `CaseTagger`, `GraphAssembler`                                  |
| `llm/`             | LLM integration (OpenRouter) | `OpenRouterClient`, `OpenRouterConfig`                                           |
| `visualization/`   | Advanced visualization       | `plot_model_state`, `CaseComparisonVisualizer`                                   |
| `examples/`        | Runnable examples            | Example scripts and demos                                                        |
| `tests/`           | Unit and integration tests   | 235+ tests                                                                       |
| `scripts/`         | Executable utilities         | `run_tests`, `run_examples`                                                      |

## Module Dependencies

```text
core/ ──────► cases/, transformations/
    │
    └──────► models/
    │
    └──────► utils/, visualization/, analysis/
    │
    └──────► examples/, scripts/

llm/ (standalone - external API integration)
lexicon/ (depends on core/)
```

## Testing

Run all tests:

```bash
python -m pytest src/tests/ -v
```

Run specific module tests:

```bash
python -m pytest src/tests/test_model.py -v
```
