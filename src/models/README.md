# Models

This directory contains concrete implementations of cognitive models using the CEREBRUM framework.

## Contents

| File/Directory | Purpose |
| -------------- | ------- |
| `__init__.py` | Module exports |
| `base.py` | Base model classes and Case enum |
| `case_definitions.py` | Case-specific behavior definitions |
| `linear_regression.py` | Linear regression model with case support |
| `insect/` | Specialized insect cognition models |
| `insect/base.py` | InsectModel base class |
| `insect/species.py` | Species-specific models (Ant, Bee, Moth) |
| `insect/behaviors.py` | Behavioral repertoire |
| `insect/neural_structures.py` | Neural architecture components |
| `insect/cases/` | Insect-specific case implementations |

## Model Hierarchy

```text
Model (src.core.model)
├── ActiveInferenceModel (src.core.active_inference)
├── NeuralNetworkModel (src.core.neural_network)
├── LinearRegressionModel (src.models.linear_regression)
└── InsectModel (src.models.insect.base)
    ├── AntModel
    ├── BeeModel
    └── MothModel
```
