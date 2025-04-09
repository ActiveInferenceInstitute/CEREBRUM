import numpy as np
from enum import Enum
from typing import Dict, Any, Optional, List, Callable, Tuple, Union
import uuid
import logging

# Configure logging (can be configured more robustly elsewhere)
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')

class Case(Enum):
    """Enumeration of all possible cases a Model can be in"""
    NOMINATIVE = "NOM"  # Model as active agent
    ACCUSATIVE = "ACC"  # Model as object of process
    GENITIVE = "GEN"    # Model as source/possessor
    DATIVE = "DAT"      # Model as recipient
    INSTRUMENTAL = "INS"  # Model as method/tool
    LOCATIVE = "LOC"    # Model as context
    ABLATIVE = "ABL"    # Model as origin/cause
    VOCATIVE = "VOC"    # Model as addressable entity


class Model:
    """
    Base class for all generative models in the CEREBRUM framework.
    
    Models can exist in different cases, which change their functional role
    while preserving their core identity.
    """
    
    def __init__(self, name: str = None, parameters: Dict[str, Any] = None):
        """
        Initialize a Model with a name and parameters.
        
        Args:
            name: A unique identifier for the model
            parameters: Initial parameters for the model
        """
        self.id = str(uuid.uuid4())
        self.name = name or f"Model_{self.id[:8]}"
        self.parameters = parameters or {}
        self._case = Case.NOMINATIVE  # Default case is NOMINATIVE
        self._prior_case = None
        self._case_history = []
        
        # Each case has its own parameter access patterns and interface configurations
        self._case_configurations = {case: {} for case in Case}
        
        # Initialize with default precision weights for each case
        self._precision_weights = {case: 1.0 for case in Case}
        
        # Track connections to other models
        self.connections = []
    
    @property
    def case(self) -> Case:
        """Get the current case of the model"""
        return self._case
    
    @case.setter
    def case(self, new_case: Case):
        """
        Change the case of the model, which transforms its functional role.
        
        Args:
            new_case: The new case to set for this model
        """
        if new_case != self._case:
            self._prior_case = self._case
            old_case_value = self._case.value
            self._case = new_case
            self._case_history.append((self._prior_case, new_case))
            logging.info(f"Model '{self.name}' ({self.id}): Case changed from {old_case_value} to {new_case.value}")
            self._apply_case_transformation()
    
    def _apply_case_transformation(self):
        """Apply transformations when changing between cases"""
        # This should be overridden by subclasses to implement specific transformations
        # between cases, including parameter access, interface changes, etc.
        pass
    
    def set_precision(self, case: Case, precision: float):
        """
        Set the precision weighting for a specific case.
        
        Args:
            case: The case to set precision for
            precision: The precision value (higher = more weight)
        """
        self._precision_weights[case] = precision
    
    def get_precision(self, case: Optional[Case] = None) -> float:
        """
        Get the precision weighting for a case.
        
        Args:
            case: The case to get precision for, or current case if None
            
        Returns:
            The precision weight value
        """
        if case is None:
            case = self._case
        return self._precision_weights[case]
    
    def connect(self, other_model: 'Model', relation_type: str = None):
        """
        Connect this model to another model with a specified relation type.
        
        Args:
            other_model: The model to connect to
            relation_type: The type of connection/relation
        """
        self.connections.append((other_model, relation_type))
    
    def free_energy(self) -> float:
        """
        Calculate the variational free energy of the model.
        
        Returns:
            The free energy value (to be minimized)
        """
        # To be implemented by subclasses based on their specific generative models
        raise NotImplementedError("Subclasses must implement free_energy()")
    
    def update(self, data: Any = None) -> Dict[str, Any]:
        """
        Update the model based on data and current case.
        
        Args:
            data: Input data for the update
            
        Returns:
            Dictionary of update results
        """
        # This generic update method will dispatch to case-specific methods
        if self._case == Case.NOMINATIVE:
            return self._update_nominative(data)
        elif self._case == Case.ACCUSATIVE:
            return self._update_accusative(data)
        elif self._case == Case.GENITIVE:
            return self._update_genitive(data)
        elif self._case == Case.DATIVE:
            return self._update_dative(data)
        elif self._case == Case.INSTRUMENTAL:
            return self._update_instrumental(data)
        elif self._case == Case.LOCATIVE:
            return self._update_locative(data)
        elif self._case == Case.ABLATIVE:
            return self._update_ablative(data)
        elif self._case == Case.VOCATIVE:
            return self._update_vocative(data)
        
        return {"status": "error", "message": f"Unknown case {self._case}"}
    
    # Case-specific update methods to be implemented by subclasses
    def _update_nominative(self, data: Any) -> Dict[str, Any]:
        """Update for NOMINATIVE case: model as active agent generating predictions"""
        raise NotImplementedError()
    
    def _update_accusative(self, data: Any) -> Dict[str, Any]:
        """Update for ACCUSATIVE case: model as object receiving updates"""
        raise NotImplementedError()
    
    def _update_genitive(self, data: Any) -> Dict[str, Any]:
        """Update for GENITIVE case: model as source/generator of outputs"""
        raise NotImplementedError()
    
    def _update_dative(self, data: Any) -> Dict[str, Any]:
        """Update for DATIVE case: model as recipient of data flows"""
        raise NotImplementedError()
    
    def _update_instrumental(self, data: Any) -> Dict[str, Any]:
        """Update for INSTRUMENTAL case: model as method/tool"""
        raise NotImplementedError()
    
    def _update_locative(self, data: Any) -> Dict[str, Any]:
        """Update for LOCATIVE case: model as context"""
        raise NotImplementedError()
    
    def _update_ablative(self, data: Any) -> Dict[str, Any]:
        """Update for ABLATIVE case: model as origin/cause"""
        raise NotImplementedError()
    
    def _update_vocative(self, data: Any) -> Dict[str, Any]:
        """Update for VOCATIVE case: model as addressable entity"""
        raise NotImplementedError()
    
    def __repr__(self):
        """String representation of the model"""
        return f"{self.name}[{self._case.value}]" 