# CEREBRUM Source Package Context

## Version

**1.5.0** - Comprehensive methods review (Feb 2026)

## Architecture

```text
src/
├── core/            # Model, Case, ActiveInferenceModel, NeuralNetworkModel, ModelRegistry
├── cases/           # Case implementations and CaseManager
├── models/          # Cognitive models (insect, animal, toy)
├── transformations/ # Case transformation logic
├── utils/           # Data generation, visualization, animation
├── analysis/        # Simulation assessment tools
├── visualization/   # Advanced visualization
├── lexicon/         # Linguistic analysis (LexiconEngine)
├── llm/             # LLM integration (OpenRouter, Perplexity)
├── examples/        # Usage examples
├── scripts/         # Test runners
└── tests/           # 253 tests
```

## Dependency Flow

```text
core → cases → models → transformations
         ↓         ↓           ↓
      utils ← visualization ← analysis
```

## Quick Start

```python
from src import Model, Case, ActiveInferenceModel, NeuralNetworkModel
from src import get_global_registry, register_model
from src.utils import DataGenerator
from src.cases import CaseManager
from src.transformations import transform_case

# Create and register a model
model = NeuralNetworkModel(input_dim=10, output_dim=2)
register_model(model, tags=["neural", "classification"])

# Generate data
X, y = DataGenerator.classification_data(n_samples=100, n_classes=2)

# Train
model.train(X, y, epochs=100)

# Transform case
model.case = Case.GENITIVE  # Model as source/generator
```

## Key Exports (src/**init**.py)

| Export | Type | Purpose |
| ------ | ---- | ------- |
| `Model` | Class | Base model class |
| `Case` | Enum | 8 linguistic cases |
| `ActiveInferenceModel` | Class | Active inference |
| `NeuralNetworkModel` | Class | Neural networks |
| `ModelRegistry` | Class | Model lifecycle |
| `get_global_registry()` | Function | Get global registry |
| `register_model()` | Function | Register a model |
| `get_model()` | Function | Retrieve a model |

## Recent Improvements (v1.5.0)

### Core Module

- Added `Model.get_state()` method
- Added `Model.set_parameters()` method
- Added `Model.get_case_history()` method
- Added `Model.predict()` abstract method
- Default implementations for all `_update_*` methods

### Utils Module

- Added `DataGenerator.classification_data()`
- Added `DataGenerator.multivariate_data()`
- Added `DataGenerator.time_series_data()`
- Added `DataGenerator.split_data()`

### Testing

- 253 tests total (up from 235)
- 0 warnings
- 18 new tests for new methods

## Testing

```bash
# Run all tests (0 warnings)
python -m pytest src/tests/ -v

# Quick smoke test
python -c "from src import Model, Case; print(Model().get_state())"
```

## Patterns

1. **Thin Orchestrator**: Scripts delegate to module functions
2. **Lazy Loading**: Optional dependencies loaded on demand
3. **`__all__` Exports**: Explicit public APIs
4. **Numerical Stability**: Gradient clipping in neural networks
