"""
Genitive case implementation for CEREBRUM framework.

This module provides specialized methods for handling the genitive case,
where a model serves as a source of information, parameters, or derivation.
"""

from typing import Dict, Any, Optional, List, Tuple, Union, Callable
import numpy as np
import logging

from ..core.model import Model, Case


class GenitiveCase:
    """
    Genitive case handler for models serving as sources or origins.
    
    The genitive case represents the model as a source of information,
    configuration, or derivation for other models.
    """
    
    # Precision modifier for Active Inference
    PRECISION = 1.0  # Baseline precision for source/origin
    
    @staticmethod
    def apply(model: Model) -> Model:
        """
        Apply genitive case configuration to a model.
        
        Args:
            model: The model to configure for genitive case
            
        Returns:
            The configured model
        """
        model.case = Case.GENITIVE
        
        # Genitive-specific: ensure model can provide parameters/state
        if not hasattr(model, '_genitive_exports'):
            model._genitive_exports = set()
        
        logging.info(f"Applied genitive case to model {model.name}")
        return model
    
    @staticmethod
    def process_update(model: Model, data: Any) -> Dict[str, Any]:
        """
        Process an update in the genitive case.
        
        Genitive updates typically involve exporting or deriving information
        for other models to consume.
        
        Args:
            model: The model being updated
            data: The update data
            
        Returns:
            Update result dictionary with exported data
        """
        if hasattr(model, '_update_genitive') and callable(getattr(model, '_update_genitive')):
            return model._update_genitive(data)
        
        result = {
            "status": "source",
            "role": "providing"
        }
        
        # Genitive behavior: export parameters and state
        if hasattr(model, 'export_parameters') and callable(getattr(model, 'export_parameters')):
            result["exported_parameters"] = model.export_parameters()
        
        if hasattr(model, 'get_derived_state') and callable(getattr(model, 'get_derived_state')):
            result["derived_state"] = model.get_derived_state(data)
        
        return result
    
    @staticmethod
    def calculate_free_energy(model: Model) -> float:
        """
        Calculate free energy for genitive case.
        
        In the genitive case, free energy relates to the coherence
        and quality of information being provided as a source.
        
        Args:
            model: The model to calculate free energy for
            
        Returns:
            The calculated free energy
        """
        default_fe = 1.0
        
        # Genitive: measure consistency of exported information
        if hasattr(model, 'parameters') and hasattr(model, 'expected_parameters'):
            try:
                params = model.parameters
                expected = model.expected_parameters
                
                # Calculate divergence between actual and expected parameters
                if isinstance(params, dict) and isinstance(expected, dict):
                    divergence = 0.0
                    for key in expected:
                        if key in params:
                            actual = params[key]
                            exp = expected[key]
                            if isinstance(actual, (int, float)) and isinstance(exp, (int, float)):
                                divergence += (actual - exp) ** 2
                    return divergence * GenitiveCase.PRECISION
            except Exception as e:
                logging.warning(f"Error calculating genitive free energy: {e}")
                return default_fe
        
        return default_fe
    
    @staticmethod
    def get_parameters(model: Model) -> Dict[str, Any]:
        """
        Get parameters relevant to genitive case.
        
        Args:
            model: The model to get parameters from
            
        Returns:
            Dictionary of genitive case parameters
        """
        params = {
            "export_precision": model.parameters.get("export_precision", 1.0),
            "derivation_depth": model.parameters.get("derivation_depth", 2),
            "inheritance_mode": model.parameters.get("inheritance_mode", "selective"),
            "provenance_tracking": model.parameters.get("provenance_tracking", True)
        }
        
        return params
    
    @staticmethod
    def derive_from(source: Model, target: Model, 
                   attributes: Optional[List[str]] = None) -> Dict[str, Any]:
        """
        Derive information from source model to target model.
        
        This is a key genitive operation: transferring/inheriting data.
        
        Args:
            source: The genitive source model
            target: The receiving model
            attributes: Optional list of specific attributes to derive
            
        Returns:
            Dictionary of derived information
        """
        derived = {}
        
        if attributes is None:
            # Default attributes to derive
            attributes = ['parameters', 'configuration', 'state']
        
        for attr in attributes:
            if hasattr(source, attr):
                value = getattr(source, attr)
                if isinstance(value, dict):
                    derived[attr] = value.copy()
                else:
                    derived[attr] = value
        
        logging.info(f"Derived {len(derived)} attributes from {source.name} to {target.name}")
        return derived
