"""
Nominative case implementation for CEREBRUM framework.

This module provides specialized methods for handling the nominative case,
where a model acts as an active agent making decisions and generating actions.
"""

from typing import Dict, Any
import numpy as np
import logging

from ..core.model import Model, Case

logger = logging.getLogger(__name__)

class NominativeCase:
    """
    Nominative case handler for models acting as active agents.

    The nominative case represents the model as a decision-making entity
    that generates actions and initiates processes.
    """

    PRECISION = 1.5  # Higher precision for active agent (confident predictions)

    @staticmethod
    def apply(model: Model) -> Model:
        """Apply nominative case to model, enabling active agent behavior."""
        # Set the case through the model's case property
        # This will trigger _apply_case_transformation internally
        model.case = Case.NOMINATIVE
        
        # Additional nominative-specific initializations can be done here
        # For example, ensure any action generation mechanisms are enabled
        
        logger.info(f"Applied nominative case to model {model.name}")
        return model
    
    @staticmethod
    def process_update(model: Model, data: Any) -> Dict[str, Any]:
        """Process a nominative-case update, delegating to _update_nominative if present."""
        # Call the model's internal nominative update method if it exists
        if hasattr(model, '_update_nominative') and callable(getattr(model, '_update_nominative')):
            return model._update_nominative(data)
        
        # Default implementation for models without their own method
        result = {
            "status": "active",
            "action": "generated"
        }
        
        # Standard nominative behavior: goal-directed action
        if hasattr(model, 'generate_action') and callable(getattr(model, 'generate_action')):
            action = model.generate_action(data)
            result["action_details"] = action
        
        return result
    
    @staticmethod
    def calculate_free_energy(model: Model) -> float:
        """Calculate free energy as divergence between goal_state and current_state."""
        # Default implementation - models should override this
        # Nominative case typically has higher precision for prediction errors
        default_fe = 1.0
        
        # If the model has a goal or desired state, calculate distance to it
        if hasattr(model, 'goal_state') and hasattr(model, 'current_state'):
            try:
                goal = getattr(model, 'goal_state')
                current = getattr(model, 'current_state')
                
                # Simple Euclidean distance if states are array-like
                if hasattr(goal, 'shape') and hasattr(current, 'shape'):
                    return np.linalg.norm(current - goal)
                # Otherwise, return a binary match/no-match
                else:
                    return 0.0 if goal == current else 1.0
            except (TypeError, ValueError) as e:
                logger.warning(f"Error calculating free energy: {e}")
                return default_fe
                
        return default_fe
    
    @staticmethod
    def get_parameters(model: Model) -> Dict[str, Any]:
        """Return nominative-relevant parameters: action_threshold, prediction_horizon, action_cost, exploration_rate."""
        # Extract nominative-specific parameters with defaults
        params = {
            "action_threshold": model.parameters.get("action_threshold", 0.5),
            "prediction_horizon": model.parameters.get("prediction_horizon", 3),
            "action_cost": model.parameters.get("action_cost", 0.1),
            "exploration_rate": model.parameters.get("exploration_rate", 0.2)
        }
        
        return params 