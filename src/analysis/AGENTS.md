# Analysis Context

## Purpose

Simulation assessment and analysis utilities for CEREBRUM models.

## Key Components

| Component | File | Purpose |
| --------- | ---- | ------- |
| `SimulationEffectivenessAnalyzer` | `simulation_assessment.py` | Core assessment functionality |
| `run_comprehensive_assessment` | `simulation_assessment.py` | Run full analysis suite |

## Key Exports

```python
from src.analysis import SimulationEffectivenessAnalyzer, run_comprehensive_assessment
```

## Lazy Loading

This module uses lazy loading for optional visualization dependencies:

- `matplotlib` and `seaborn` are only imported when needed
- Analysis functions work without visualization libraries

## Usage Example

```python
from src.analysis import SimulationEffectivenessAnalyzer

analyzer = SimulationEffectivenessAnalyzer()
results = analyzer.analyze(model, simulation_data)
```

## Metrics

The analyzer provides:

- Belief accuracy metrics
- Free energy calculations
- Prediction error distributions
- Case-specific performance breakdowns
