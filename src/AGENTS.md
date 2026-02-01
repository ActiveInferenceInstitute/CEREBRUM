# Source Code Context

## Architecture

The `src/` directory is the heart of CEREBRUM, organized into modular layers:

```text
src/
├── core/          # Foundation: Model, Case, ActiveInferenceModel
├── cases/         # Case implementations (NOM, ACC, GEN, DAT, INS, LOC, ABL, VOC)
├── models/        # Cognitive models (ToyModel, insect models, neural networks)
├── transformations/ # Case transformation logic
├── utils/         # Utilities, animation, visualization
├── analysis/      # Simulation assessment and metrics
├── lexicon/       # NLP and linguistic analysis
├── llm/           # LLM integration (OpenRouter)
├── visualization/ # Advanced visualization tools
├── examples/      # Runnable examples
├── scripts/       # Test runners and utilities
└── tests/         # Unit and integration tests
```

## Dependency Flow

```text
core → [cases, transformations]
     → [models]
     → [utils, visualization, analysis]
     → [examples, scripts]
```

- `core` should not depend on `models` or `examples`
- `models` depends on `core` and `cases`
- `examples` depends on everything
- `llm` is standalone (external API integration)

## Key Imports

```python
from src.core import Model, Case, ActiveInferenceModel
from src.cases import CaseManager
from src.models.insect.base import InsectModel
from src.analysis import SimulationEffectivenessAnalyzer
```

## Testing

All changes in `src/` should be accompanied by tests in `src/tests/`:

- `test_model.py` - Core model tests
- `test_environment.py` - Environment tests
- `test_case_transformations.py` - Case logic tests
- `test_insect_models.py` - Insect model tests (800+ lines)
- `test_analysis.py` - Analysis module tests
