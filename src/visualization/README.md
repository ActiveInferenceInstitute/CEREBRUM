# Visualization

Specialized visualization tools for CEREBRUM.

## Contents

| File/Directory | Purpose |
| -------------- | ------- |
| `__init__.py` | Module exports (lazy loading) |
| `case_visualization.py` | Case structure visualization |
| `case_comparison.py` | Cross-case comparison visualizer |
| `animal_visualization.py` | Animal/agent state visualization |
| `insect/` | Insect-specific visualizers (9 modules) |
| `insect/animation_creator.py` | Simulation animations |
| `insect/behavior_visualizer.py` | Behavioral state plots |
| `insect/case_visualizer.py` | Case transition plots |
| `insect/comprehensive_visualizer.py` | Full-suite visualizations |
| `insect/insect_visualizer.py` | Core insect plots |
| `insect/neural_visualizer.py` | Neural structure plots |
| `insect/report_generator.py` | Report generation |
| `insect/simulation_logger.py` | Simulation logging |

## Usage

```python
from src.visualization import plot_model_state, CaseComparisonVisualizer

# Quick model snapshot
plot_model_state(model, output_dir="output/")

# Cross-case comparison
viz = CaseComparisonVisualizer()
viz.compare(models, output_dir="output/comparisons/")
```
