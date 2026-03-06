# Utils

Utilities for data generation, visualization helpers, animation, and file management.

## Contents

| File/Directory | Purpose |
| -------------- | ------- |
| `__init__.py` | Module exports (lazy loading) |
| `array_utils.py` | Array validation and normalization |
| `data_generator.py` | DataGenerator class for synthetic data |
| `visualization.py` | Visualizer and plotting functions |
| `animation.py` | Animation creation utilities |
| `fix_animations.py` | Animation repair utilities |
| `fix_case_animation.py` | Case-specific animation fixes |
| `output_organizer.py` | Output directory organization |
| `path_utils.py` | Output/log directory management |
| `markdown/` | Markdown processing utilities |

## Quick Start

```python
from src.utils import DataGenerator, Visualizer

# Generate data
X, y = DataGenerator.linear_data(n_samples=100, slope=2.0)
X, y = DataGenerator.classification_data(n_samples=100, n_classes=3)

# Visualize
Visualizer.plot_regression(X, y, title="Linear Data")
```
