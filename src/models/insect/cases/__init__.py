"""
Insect-Specific CEREBRUM Cases Package

This package provides implementations of insect-specific CEREBRUM cases that extend
the standard case system with specialized functionality for insect cognition.
"""

from .pheromonal import PheromonalCase
from .swarm import SwarmCase
from .metamorphic import MetamorphicCase
from .caste import CasteCase
from .substrate import SubstrateCase
from .stigmergic import StigmergicCase

__all__ = [
    'PheromonalCase',
    'SwarmCase', 
    'MetamorphicCase',
    'CasteCase',
    'SubstrateCase',
    'StigmergicCase'
]

__version__ = "0.1.0" 