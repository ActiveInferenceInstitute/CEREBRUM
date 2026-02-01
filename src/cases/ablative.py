"""
Ablative case implementation for CEREBRUM framework.

This module provides specialized methods for handling the ablative case,
where a model serves as an origin, source of motion, or starting point.
"""

from typing import Dict, Any, Optional, List, Tuple, Union, Callable
import numpy as np
import logging

from ..core.model import Model, Case


class AblativeCase:
    """
    Ablative case handler for models serving as origins or sources.
    
    The ablative case represents the model as a point of origin
    from which something moves away or is derived.
    """
    
    # Precision modifier for Active Inference
    PRECISION = 1.1  # Moderate precision for source/origin
    
    # Valid transitions from ablative case
    VALID_TRANSITIONS = [Case.NOMINATIVE]
    
    @staticmethod
    def apply(model: Model) -> Model:
        """
        Apply ablative case configuration to a model.
        
        Args:
            model: The model to configure for ablative case
            
        Returns:
            The configured model
        """
        model.case = Case.ABLATIVE
        
        # Ablative-specific: track what has originated from this model
        if not hasattr(model, '_ablative_emissions'):
            model._ablative_emissions = []
        
        logging.info(f"Applied ablative case to model {model.name}")
        return model
    
    @staticmethod
    def process_update(model: Model, data: Any) -> Dict[str, Any]:
        """
        Process an update in the ablative case.
        
        Ablative updates involve emitting or originating data/state
        for other processes to consume.
        
        Args:
            model: The model being updated
            data: The update data
            
        Returns:
            Update result dictionary with emission data
        """
        if hasattr(model, '_update_ablative') and callable(getattr(model, '_update_ablative')):
            return model._update_ablative(data)
        
        result = {
            "status": "origin",
            "role": "emitting"
        }
        
        # Ablative behavior: emit state or data
        if hasattr(model, 'emit') and callable(getattr(model, 'emit')):
            emission = model.emit(data)
            result["emission"] = emission
            if hasattr(model, '_ablative_emissions'):
                model._ablative_emissions.append(emission)
        
        return result
    
    @staticmethod
    def calculate_free_energy(model: Model) -> float:
        """
        Calculate free energy for ablative case.
        
        In the ablative case, free energy relates to the consistency
        of what originates from the source.
        
        Args:
            model: The model to calculate free energy for
            
        Returns:
            The calculated free energy
        """
        default_fe = 1.0
        
        # Ablative: measure consistency of emissions
        if hasattr(model, '_ablative_emissions') and len(model._ablative_emissions) > 1:
            try:
                emissions = model._ablative_emissions[-10:]  # Last 10 emissions
                if all(isinstance(e, (int, float)) for e in emissions):
                    variance = np.var(emissions)
                    return variance * AblativeCase.PRECISION
            except Exception as e:
                logging.warning(f"Error calculating ablative free energy: {e}")
                return default_fe
        
        return default_fe
    
    @staticmethod
    def get_parameters(model: Model) -> Dict[str, Any]:
        """
        Get parameters relevant to ablative case.
        
        Args:
            model: The model to get parameters from
            
        Returns:
            Dictionary of ablative case parameters
        """
        params = {
            "emission_rate": model.parameters.get("emission_rate", 1.0),
            "origin_tracking": model.parameters.get("origin_tracking", True),
            "max_emissions": model.parameters.get("max_emissions", 1000),
            "emission_precision": model.parameters.get("emission_precision", 1.1)
        }
        
        return params
    
    @staticmethod
    def emit_to(source: Model, target: Model, 
               payload: Optional[Any] = None) -> Dict[str, Any]:
        """
        Emit data from ablative source to a target.
        
        Args:
            source: The ablative source model
            target: The receiving model
            payload: Optional data to emit (uses source state if None)
            
        Returns:
            Dictionary describing the emission
        """
        if payload is None:
            if hasattr(source, 'current_state'):
                payload = source.current_state
            elif hasattr(source, 'parameters'):
                payload = source.parameters.copy()
            else:
                payload = {"source": source.name}
        
        emission = {
            "from": source.name,
            "to": target.name,
            "payload": payload,
            "precision": AblativeCase.PRECISION
        }
        
        if hasattr(source, '_ablative_emissions'):
            source._ablative_emissions.append(emission)
        
        logging.info(f"Emitted from {source.name} to {target.name}")
        return emission
    
    @staticmethod
    def can_transition_to(target_case: Case) -> bool:
        """
        Check if ablative can transition to target case.
        
        Args:
            target_case: The target case to transition to
            
        Returns:
            True if transition is valid
        """
        return target_case in AblativeCase.VALID_TRANSITIONS
