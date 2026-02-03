# Core Module Context

## Purpose

Foundational classes for the CEREBRUM framework with case-based cognitive modeling.

## Architecture

```text
core/
├── __init__.py          # Module exports
├── model.py             # Base Model and Case enum
├── active_inference.py  # ActiveInferenceModel
├── neural_network.py    # NeuralNetworkModel
└── model_registry.py    # ModelRegistry and utilities
```

## Key Exports

```python
from src.core import (
    # Base classes
    Model,
    Case,
    ActiveInferenceModel,
    NeuralNetworkModel,
    # Registry
    ModelRegistry,
    ModelMetadata,
    # Convenience functions
    get_global_registry,
    register_model,
    get_model,
)
```

## Core Classes

| Class | Purpose | Key Methods |
| ----- | ------- | ----------- |
| `Model` | Base class for all models | `update()`, `get_state()`, `set_parameters()`, `predict()` |
| `Case` | Enum with 8 linguistic cases | `NOMINATIVE`, `ACCUSATIVE`, `GENITIVE`, etc. |
| `ActiveInferenceModel` | Active inference implementation | `update_posterior()`, `get_optimal_action()`, `free_energy()` |
| `NeuralNetworkModel` | Neural network model | `forward()`, `backward()`, `train()`, `evaluate()` |
| `ModelRegistry` | Model lifecycle management | `register_model()`, `get_model()`, `list_models()` |

## Model Methods

All models inherit from `Model` and provide:

| Method | Purpose |
| ------ | ------- |
| `update(data)` | Dispatches to case-specific update |
| `get_state()` | Returns model state dictionary |
| `set_parameters(params)` | Updates model parameters |
| `get_case_history()` | Returns list of case transitions |
| `predict(data)` | Make prediction (override in subclasses) |
| `free_energy()` | Calculate variational free energy |

## Numerical Stability

Neural network contains gradient clipping to prevent overflow:

- Forward pass: clips activations to `[-500, 500]`
- Backward pass: clips gradients to `[-1e5, 1e5]`
