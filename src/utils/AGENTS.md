# Utils Module Context

## Purpose

Utilities for data generation, visualization, and file management.

## Architecture

```text
utils/
├── __init__.py          # Module exports (lazy loading)
├── data_generator.py    # DataGenerator class
├── visualization.py     # Visualizer and plotting functions
├── animation.py         # Animation utilities
├── path_utils.py        # Output directory management
└── markdown/            # Markdown processing utilities
```

## Key Exports

```python
from src.utils import (
    DataGenerator,
    Visualizer,
    plot_case_linguistic_context,
    save_animation,
    save_frames_as_gif,
    create_linear_regression_animation,
    ensure_scalar,
    get_output_dir,
    save_plot,
)
```

## DataGenerator Methods

| Method | Purpose |
| ------ | ------- |
| `linear_data()` | Generate linear regression data |
| `polynomial_data()` | Generate polynomial regression data |
| `classification_data()` | Generate classification datasets |
| `multivariate_data()` | Multi-feature regression data |
| `time_series_data()` | Time series with trends |
| `split_data()` | Train/test split utility |
| `normalize_arrays()` | Ensure consistent array shapes |

## Usage

```python
from src.utils import DataGenerator

# Generate linear data
X, y = DataGenerator.linear_data(n_samples=100, slope=2.0)

# Generate classification data
X, y = DataGenerator.classification_data(n_samples=100, n_classes=3)

# Generate time series
t, y = DataGenerator.time_series_data(n_samples=100, trend=0.1)

# Split data
X_train, X_test, y_train, y_test = DataGenerator.split_data(X, y, test_size=0.2)
```

## Lazy Loading

This module uses `__getattr__` for lazy loading to avoid importing optional dependencies (like `imageio`) until needed.
