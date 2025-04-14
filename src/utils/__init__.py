"""
CEREBRUM Utilities Package
Contains utility functions and classes for data generation and visualization
"""

from .data_generator import DataGenerator
from .visualization import Visualizer, plot_case_linguistic_context
from .animation import (
    save_animation, 
    save_frames_as_gif, 
    create_linear_regression_animation, 
    ensure_scalar
)

__all__ = [
    'DataGenerator', 
    'Visualizer', 
    'plot_case_linguistic_context',
    'save_animation', 
    'save_frames_as_gif', 
    'create_linear_regression_animation',
    'ensure_scalar'
]
