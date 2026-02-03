# CEREBRUM Source Package
"""
CEREBRUM: Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling

This package provides the core implementation of the CEREBRUM framework,
integrating linguistic case systems with cognitive scientific principles.

Quick Start:
    from src import Model, Case, ActiveInferenceModel, NeuralNetworkModel
    from src.core import ModelRegistry, get_global_registry, register_model
    from src.cases import NominativeCase, AccusativeCase, CaseManager
    from src.transformations import transform_case, revert_case
    from src.utils import DataGenerator, Visualizer, get_output_dir
    from src.analysis import SimulationEffectivenessAnalyzer

Modules:
    core: Base classes (Model, Case, ActiveInferenceModel, NeuralNetworkModel, ModelRegistry)
    cases: Case-specific implementations and CaseManager
    models: Cognitive model implementations (insect, animal, toy)
    transformations: Case transformation logic
    utils: Utilities, data generation, and animation
    analysis: Simulation assessment tools
    lexicon: Linguistic analysis tools (LexiconEngine)
    llm: Large Language Model integration (OpenRouter, Perplexity)
    visualization: Advanced visualization tools
"""

__version__ = "1.5.0"
__author__ = "Daniel Ari Friedman"
__email__ = "daniel@activeinference.institute"

# Expose core classes at package level for convenience
from .core import (
    Model,
    Case,
    ActiveInferenceModel,
    NeuralNetworkModel,
    ModelRegistry,
    get_global_registry,
    register_model,
    get_model,
)

__all__ = [
    '__version__',
    '__author__',
    '__email__',
    # Core classes
    'Model',
    'Case',
    'ActiveInferenceModel',
    'NeuralNetworkModel',
    'ModelRegistry',
    # Convenience functions
    'get_global_registry',
    'register_model',
    'get_model',
]
