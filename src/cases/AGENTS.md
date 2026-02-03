# Cases Context

## Purpose

The `cases/` module implements the eight linguistic cases used in CEREBRUM.

## Cases Overview

| Case | Abbreviation | Role | Example |
| ---- | ------------ | ---- | ------- |
| Nominative | NOM | Agent/Subject | Model predicts outcomes |
| Accusative | ACC | Patient/Object | Model being evaluated |
| Genitive | GEN | Source/Possessor | Model generates beliefs |
| Dative | DAT | Recipient/Goal | Model receives updates |
| Instrumental | INS | Means/Method | Model as computational tool |
| Locative | LOC | Location/Position | Model in parameter space |
| Ablative | ABL | Origin/Separation | Model as source of uncertainty |
| Vocative | VOC | Address/Communication | Model as communication channel |

## Key Components

| Component | File | Purpose |
| --------- | ---- | ------- |
| `CaseManager` | `case_manager.py` | Orchestrates case state transitions |
| `NominativeCase` | `nominative.py` | Agent/Subject case implementation |
| `AccusativeCase` | `accusative.py` | Patient/Object case implementation |
| `AnimalCaseManager` | `animal_case_manager.py` | Specialized for animal models |

## Key Exports

```python
from src.cases import (
    Case, CaseManager, NominativeCase, AccusativeCase,
    DativeCase, GenitiveCase, InstrumentalCase, LocativeCase,
    AblativeCase, VocativeCase, AnimalCaseManager
)
```

## Usage Guidelines

1. Cases act as "wrappers" or "states" for Models
2. Use `CaseManager` to orchestrate case transitions
3. Never directly modify `model.case` - use `transform_case()` from transformations
4. Each case handler implements `apply()` and `process_update()` methods
