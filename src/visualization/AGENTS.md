# Visualization Context

## Purpose

Advanced visualization tools for CEREBRUM, kept separate from core logic.

## Architecture

- Visualizers subscribe to model updates or take model snapshots as input
- Visualization code is decoupled from core to keep `src/core/` clean
- Heavy visualization dependencies are lazily loaded

## Subdirectories

| Directory | Purpose |
| --------- | ------- |
| `insect/` | Visualizers specific to insect models |

## Key Exports

```python
from src.visualization import (
    # Core visualization
    plot_model_state, plot_case_dynamics, create_case_transition_animation,
    
    # Case comparison
    CaseComparisonVisualizer, CaseTransitionVisualizer, plot_case_comparison,
    
    # Animal visualization
    plot_animal_states, create_animal_animation, AnimalVisualizationConfig,
    
    # Belief visualization
    plot_belief_state, plot_belief_evolution
)
```

## Conditional Imports

Insect-specific visualizations are conditionally available:

```python
from src.visualization import HAS_INSECT_VISUALIZATION

if HAS_INSECT_VISUALIZATION:
    from src.visualization import plot_ant_colony, plot_bee_swarm
```

## Usage Guidelines

1. Use `plot_model_state()` for quick model snapshots
2. Use `CaseComparisonVisualizer` for case-by-case analysis
3. Always close figures with `plt.close(fig)` to prevent memory leaks
4. Animation functions require the `animation` module from `utils/`
