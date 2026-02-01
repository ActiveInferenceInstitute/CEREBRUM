"""
Case-specific implementations for the CEREBRUM framework.

This module provides specialized case implementations and case-based reasoning methods.

Available Cases:
- NominativeCase: Model as active agent
- AccusativeCase: Model as object of process
- DativeCase: Model as recipient
- GenitiveCase: Model as source/origin
- InstrumentalCase: Model as tool/mechanism
- AblativeCase: Model as origin of motion
- LocativeCase: Model as context/container
- VocativeCase: Model as target of address
"""

from .nominative import NominativeCase
from .accusative import AccusativeCase
from .dative import DativeCase
from .genitive import GenitiveCase
from .instrumental import InstrumentalCase
from .ablative import AblativeCase
from .locative import LocativeCase
from .vocative import VocativeCase
from .case_manager import CaseManager
from .animal_cases import AnimalCaseManager

# Re-export Case enum from core for convenience
from src.core.model import Case

__all__ = [
    'Case',
    'NominativeCase',
    'AccusativeCase', 
    'DativeCase',
    'GenitiveCase',
    'InstrumentalCase',
    'AblativeCase',
    'LocativeCase',
    'VocativeCase',
    'CaseManager',
    'AnimalCaseManager',
]

