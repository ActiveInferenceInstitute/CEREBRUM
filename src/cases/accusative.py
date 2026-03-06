"""
Accusative case implementation for CEREBRUM framework.

This module provides specialized methods for handling the accusative case,
where a model acts as the object of a process, receiving updates.
"""

from typing import Dict, Any, Optional, Callable
import numpy as np
import logging

from ..core.model import Model, Case

logger = logging.getLogger(__name__)

class AccusativeCase:
    """
    Accusative case handler for models acting as objects.

    The accusative case represents the model as a recipient of actions,
    being updated or modified by other models or external processes.
    """

    PRECISION = 1.2  # Elevated precision for update reception (sensitive to incoming data)

    @staticmethod
    def apply(model: Model) -> Model:
        """Apply accusative case to model, configuring it as an object receiving updates."""
        # Set the case through the model's case property
        model.case = Case.ACCUSATIVE
        
        # Additional accusative-specific initializations
        # For example, ensure update receptors are enabled
        
        logger.info(f"Applied accusative case to model {model.name}")
        return model
    
    @staticmethod
    def process_update(model: Model, data: Any) -> Dict[str, Any]:
        """Process an accusative-case update, applying parameter changes from data dict."""
        # Call the model's internal accusative update method if it exists
        if hasattr(model, '_update_accusative') and callable(getattr(model, '_update_accusative')):
            return model._update_accusative(data)
        
        # Default implementation for models without their own method
        result = {
            "status": "updated",
            "updated_params": {}
        }
        
        # Handle parameter updates
        if isinstance(data, dict):
            for param_name, param_value in data.items():
                if param_name in model.parameters:
                    old_value = model.parameters[param_name]
                    model.parameters[param_name] = param_value
                    result["updated_params"][param_name] = {
                        "old": old_value,
                        "new": param_value
                    }
                    logger.info(f"Model {model.name} parameter updated: {param_name}={param_value}")
        
        return result
    
    @staticmethod
    def calculate_free_energy(model: Model) -> float:
        """Calculate free energy as divergence between predicted_sensory and actual_sensory."""
        # Default implementation - models should override this
        # Accusative case typically has higher precision for sensory prediction errors
        default_fe = 0.5
        
        # If the model has predicted and actual sensory states, calculate divergence
        if hasattr(model, 'predicted_sensory') and hasattr(model, 'actual_sensory'):
            try:
                predicted = getattr(model, 'predicted_sensory')
                actual = getattr(model, 'actual_sensory')
                
                # Simple Euclidean distance if states are array-like
                if hasattr(predicted, 'shape') and hasattr(actual, 'shape'):
                    return np.linalg.norm(actual - predicted)
                # Otherwise, return a binary match/no-match
                else:
                    return 0.0 if predicted == actual else 0.5
            except (AttributeError, TypeError, ValueError) as e:
                logger.warning(f"Error calculating free energy: {e}")
                return default_fe
                
        return default_fe
    
    @staticmethod
    def accept_updates(model: Model, updates: Dict[str, Any],
                      validation_func: Optional[Callable] = None) -> Dict[str, Any]:
        """Apply parameter updates to model, optionally validating each via validation_func(param, value)."""
        result = {
            "accepted": [],
            "rejected": [],
            "status": "success"
        }
        
        for param, value in updates.items():
            # Skip invalid parameters
            if param not in model.parameters:
                result["rejected"].append({
                    "param": param,
                    "reason": "parameter not found"
                })
                continue
                
            # Validate if a validation function was provided
            if validation_func is not None:
                try:
                    valid, reason = validation_func(param, value)
                    if not valid:
                        result["rejected"].append({
                            "param": param,
                            "value": value,
                            "reason": reason
                        })
                        continue
                except (AttributeError, TypeError, ValueError) as e:
                    result["rejected"].append({
                        "param": param,
                        "value": value,
                        "reason": f"validation error: {str(e)}"
                    })
                    continue
            
            # Apply the update
            old_value = model.parameters[param]
            model.parameters[param] = value
            result["accepted"].append({
                "param": param,
                "old_value": old_value,
                "new_value": value
            })
        
        if not result["accepted"] and result["rejected"]:
            result["status"] = "error"
            
        return result 