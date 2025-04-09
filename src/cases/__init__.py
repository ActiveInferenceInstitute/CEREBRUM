"""
Case-specific implementations for the CEREBRUM framework.

This module provides specialized case implementations and case-based reasoning methods.
"""

from .nominative import NominativeCase
from .accusative import AccusativeCase
from .genitive import GenitiveCase
from .dative import DativeCase
from .instrumental import InstrumentalCase
from .locative import LocativeCase
from .ablative import AblativeCase
from .vocative import VocativeCase
from .case_manager import CaseManager
