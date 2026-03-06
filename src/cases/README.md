# Cases

This directory defines the grammatical cases used in the CEREBRUM system.

## Contents

| File | Purpose |
| ---- | ------- |
| `__init__.py` | Module exports |
| `case_manager.py` | Orchestrates case state transitions |
| `nominative.py` | Agent/Subject case (NOM) |
| `accusative.py` | Patient/Object case (ACC) |
| `dative.py` | Recipient/Goal case (DAT) |
| `genitive.py` | Source/Possessor case (GEN) |
| `instrumental.py` | Means/Method case (INS) |
| `locative.py` | Location/Position case (LOC) |
| `ablative.py` | Origin/Separation case (ABL) |
| `vocative.py` | Address/Communication case (VOC) |
| `animal_cases.py` | Animal-specific case implementations |
| `examples.py` | Case usage examples |

## Usage

```python
from src.cases import CaseManager, NominativeCase, AccusativeCase
from src.transformations import transform_case

# Use CaseManager for case transitions
manager = CaseManager(model)
manager.transition(Case.ACCUSATIVE)
```
