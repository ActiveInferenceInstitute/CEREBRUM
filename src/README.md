# Source Code

This directory contains the Python source code for CEREBRUM.

## Directory Structure

| Directory | Purpose |
|-----------|---------|
| `core/` | Foundational classes (Agent, Environment, Case) and Active Inference engine |
| `models/` | Cognitive model implementations (ToyModel, AdvancedModel, insect models) |
| `lexicon/` | Linguistic analysis tools and NLP pipeline |
| `transformations/` | Logic for transforming models between cases |
| `cases/` | Case structure definitions |
| `utils/` | Helper functions and shared utilities |
| `visualization/` | Visualization tools for models and states |
| `examples/` | Runnable examples demonstrating framework usage |
| `tests/` | Unit and integration tests (200+ test files) |
| `analysis/` | Simulation assessment and analysis tools |
| `scripts/` | Executable scripts for testing and examples |
| `llm/` | Large Language Model integration (OpenRouter) |

## Usage

```python
from src.core import GenerativeModel, ActiveInferenceEngine
from src.models import ToyModel
from src.cases import Case
```
