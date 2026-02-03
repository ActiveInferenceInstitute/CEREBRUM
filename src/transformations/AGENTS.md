# Transformations Context

## Purpose

The `transformations/` module handles case transformation logic for CEREBRUM models.

## Key Concepts

- **Transformation**: A function that converts a Model from Case A to Case B
- **Morphosyntactic Alignment**: Linguistic patterns applied to model groups
- **Case Relationship**: Semantic relationship between models in different cases

## Key Functions

| Function | Purpose |
| -------- | ------- |
| `transform_case(model, target_case)` | Transform model to a target case |
| `revert_case(model)` | Revert model to its previous case |
| `apply_morphosyntactic_alignment(models)` | Apply linguistic alignment patterns |
| `create_case_relationship(source, target, type)` | Create relationship between models |
| `convert_message_between_cases(msg, src, tgt)` | Convert messages between case formats |

## Animal-Specific Functions

| Function | Purpose |
| -------- | ------- |
| `transform_to_goal_seeker(animal)` | Optimize for goal seeking (NOMINATIVE) |
| `transform_to_explorer(animal)` | Optimize for exploration (NOM/LOC hybrid) |
| `transform_to_follower(animal, target)` | Configure following behavior (DATIVE) |
| `create_animal_formation(animals, type)` | Create coordinated formations |
| `swap_animal_roles(animal1, animal2)` | Swap case roles between animals |

## Key Exports

```python
from src.transformations import (
    transform_case, revert_case, apply_morphosyntactic_alignment,
    create_case_relationship, convert_message_between_cases,
    transform_to_goal_seeker, transform_to_explorer, transform_to_follower,
    create_animal_formation, swap_animal_roles
)
```

## Usage Guidelines

1. Always use `transform_case()` rather than directly modifying `model.case`
2. Use `revert_case()` to undo transformations (maintains history)
3. Animal transformations optimize both case and behavioral parameters
