#!/usr/bin/env python3
"""
Base Model and Case Definitions for CEREBRUM
Defines fundamental structures for linguistic case modeling
"""

import uuid
import logging
from enum import Enum

# Define Case enum for the linguistic cases
class Case(Enum):
    NOMINATIVE = "NOM"
    ACCUSATIVE = "ACC"
    DATIVE = "DAT"
    GENITIVE = "GEN"
    INSTRUMENTAL = "INS"
    LOCATIVE = "LOC"
    ABLATIVE = "ABL"
    VOCATIVE = "VOC"

class Model:
    """Base Model class for all CEREBRUM models."""
    
    def __init__(self, name: str, case: Case = Case.NOMINATIVE):
        self.id = str(uuid.uuid4())
        self.name = name
        self._case = case
        self.data_buffer = {'X': None, 'y': None}
        
    @property
    def case(self) -> Case:
        return self._case
        
    @case.setter
    def case(self, value: Case):
        if not isinstance(value, Case):
            raise TypeError(f"Expected Case enum, got {type(value)}")
        # Log the case change
        logging.info(f"Model '{self.name}' ({self.id}): Case changed from {self._case} to {value}")
        self._case = value 