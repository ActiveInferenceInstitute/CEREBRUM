# CEREBRUM Source Package
"""
CEREBRUM: Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling

This package provides the core implementation of the CEREBRUM framework,
integrating linguistic case systems with cognitive scientific principles.

Quick Start:
    from src.core import Model, Case, ActiveInferenceModel
    from src.cases import NominativeCase, AccusativeCase
    from src.models import ToyModel
    from src.utils import DataGenerator, Visualizer

Modules:
    core: Base classes (Model, Case, ActiveInferenceModel)
    cases: Case-specific implementations
    models: Cognitive model implementations
    transformations: Case transformation logic
    utils: Utilities and visualization
    analysis: Simulation assessment tools
    lexicon: Linguistic analysis tools
    llm: Large Language Model integration
    visualization: Advanced visualization tools
"""

__version__ = "1.4.0"
__author__ = "Daniel Ari Friedman"
__email__ = "daniel@activeinference.institute"

# Expose core classes at package level for convenience
from .core import Model, Case, ActiveInferenceModel

__all__ = [
    '__version__',
    '__author__',
    '__email__',
    'Model',
    'Case',
    'ActiveInferenceModel',
]
