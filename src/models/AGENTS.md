# Models Context

## Purpose

This is where the actual "agents" live - the cognitive entities of CEREBRUM.

## Model Hierarchy

```text
Model (src.core.model.Model)
├── ActiveInferenceModel (src.core.active_inference)
│   └── POMDPModel (src.examples)
├── NeuralNetworkModel (src.core.neural_network)
├── LinearRegressionModel (src.models.linear_regression)
└── InsectModel (src.models.insect.base)
    ├── AntModel
    ├── BeeModel
    └── MothModel (with metamorphic transitions)
```

## Subdirectories

| Directory | Purpose |
| --------- | ------- |
| `insect/` | Biological insect simulations with 12 specialized cases |
| `insect/cases/` | Case implementations (pheromonal, metamorphic, etc.) |

## Key Exports

```python
from src.models import Model, Case, CaseDefinitions, LinearRegressionModel
from src.models.insect.base import InsectModel
```

## Implementation Guidelines

1. All models inherit from `src.core.model.Model`
2. Override `predict()`, `update()`, and `transform()` methods
3. Use `CaseDefinitions` for case-specific behavior
4. Register models with `ModelRegistry` for tracking
