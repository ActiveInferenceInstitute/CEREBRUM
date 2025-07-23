"""
CEREBRUM Insect Models Package

This package provides implementations of insect cognitive models using the CEREBRUM case framework.
It includes base models, neural structures, insect-specific cases, and species-specific implementations.
"""

from .base import InsectModel, InsectActiveInferenceModel
from .neural_structures import (
    MushroomBody,
    CentralComplex,
    AntennalLobe,
    OpticLobe,
    SubesophagealGanglion,
    VentralNerveCord
)
from .cases import (
    PheromonalCase,
    SwarmCase,
    MetamorphicCase,
    CasteCase,
    SubstrateCase,
    StigmergicCase
)
from .behaviors import (
    ForagingBehavior,
    NavigationBehavior,
    CommunicationBehavior,
    SocialBehavior
)
from .species import (
    HoneybeeModel,
    AntModel,
    FruitFlyModel
)

__all__ = [
    # Base models
    'InsectModel',
    'InsectActiveInferenceModel',
    
    # Neural structures
    'MushroomBody',
    'CentralComplex',
    'AntennalLobe',
    'OpticLobe',
    'SubesophagealGanglion',
    'VentralNerveCord',
    
    # Insect-specific cases
    'PheromonalCase',
    'SwarmCase',
    'MetamorphicCase',
    'CasteCase',
    'SubstrateCase',
    'StigmergicCase',
    
    # Behavioral modules
    'ForagingBehavior',
    'NavigationBehavior',
    'CommunicationBehavior',
    'SocialBehavior',
    
    # Species-specific models
    'HoneybeeModel',
    'AntModel',
    'FruitFlyModel'
]

__version__ = "0.1.0"
__author__ = "CEREBRUM Development Team" 