"""
CEREBRUM Core Module

This module provides the foundational classes for the CEREBRUM framework:
- Model: Base class for generative models with case-based transformations
- Case: Enumeration of all possible cases a model can be in
- ActiveInferenceModel: Active inference implementation with belief updating
- NeuralNetworkModel: Neural network based model implementation
- ModelRegistry: Centralized model registration and management
"""

from .model import Model, Case
from .active_inference import ActiveInferenceModel
from .neural_network import NeuralNetworkModel
from .model_registry import (
    ModelRegistry,
    ModelMetadata,
    get_global_registry,
    register_model,
    get_model
)

__all__ = [
    # Core classes
    'Model',
    'Case',
    'ActiveInferenceModel',
    'NeuralNetworkModel',
    # Registry
    'ModelRegistry',
    'ModelMetadata',
    # Convenience functions
    'get_global_registry',
    'register_model',
    'get_model',
]
