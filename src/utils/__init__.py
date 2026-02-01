"""
CEREBRUM Utilities Package

Contains utility functions and classes for data generation, visualization, and markdown processing.

Submodules:
    - data_generator: Data generation utilities
    - visualization: Plotting and visualization
    - animation: Animation utilities (requires imageio)
    - markdown: Markdown processing utilities
"""

__all__ = [
    'DataGenerator',
    'Visualizer',
    'plot_case_linguistic_context',
    'save_animation',
    'save_frames_as_gif',
    'create_linear_regression_animation',
    'ensure_scalar',
    # Markdown utilities available via src.utils.markdown
]


def __getattr__(name):
    """Lazy import to avoid loading optional dependencies at package import time."""
    if name == 'DataGenerator':
        from .data_generator import DataGenerator
        return DataGenerator
    elif name == 'Visualizer':
        from .visualization import Visualizer
        return Visualizer
    elif name == 'plot_case_linguistic_context':
        from .visualization import plot_case_linguistic_context
        return plot_case_linguistic_context
    elif name in ('save_animation', 'save_frames_as_gif', 'create_linear_regression_animation', 'ensure_scalar'):
        from . import animation
        return getattr(animation, name)
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
