# Transformations

This directory contains logic for transforming models between different grammatical cases.

## Contents

| File | Purpose |
| ---- | ------- |
| `__init__.py` | Module exports |
| `case_transformations.py` | Core case transformation functions |
| `animal_transformations.py` | Animal-specific transformation functions |

## Key Functions

| Function | Purpose |
| -------- | ------- |
| `transform_case(model, target_case)` | Transform a model to a target case |
| `revert_case(model)` | Revert a model to its previous case |
| `apply_morphosyntactic_alignment(models)` | Apply linguistic alignment patterns |
| `create_case_relationship(source, target, type)` | Create relationships between models |
| `transform_to_goal_seeker(animal)` | Optimize for goal seeking |
| `transform_to_explorer(animal)` | Optimize for exploration |
| `create_animal_formation(animals, type)` | Create coordinated formations |

## Usage

```python
from src.transformations import transform_case, revert_case

# Transform model to accusative (being evaluated)
transform_case(model, Case.ACCUSATIVE)

# Revert to previous case
revert_case(model)
```
