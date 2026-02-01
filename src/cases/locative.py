"""
Locative case implementation for CEREBRUM framework.

This module provides specialized methods for handling the locative case,
where a model serves as a context, container, or location for operations.
"""

from typing import Dict, Any, Optional, List, Tuple, Union, Callable
import numpy as np
import logging

from ..core.model import Model, Case


class LocativeCase:
    """
    Locative case handler for models acting as contexts or containers.
    
    The locative case represents the model as an environment, context,
    or location in which other operations take place.
    """
    
    # Precision modifier for Active Inference
    PRECISION = 0.9  # Lower precision for context (more ambient)
    
    # Valid transitions from locative case
    VALID_TRANSITIONS = [Case.ABLATIVE]
    
    @staticmethod
    def apply(model: Model) -> Model:
        """
        Apply locative case configuration to a model.
        
        Args:
            model: The model to configure for locative case
            
        Returns:
            The configured model
        """
        model.case = Case.LOCATIVE
        
        # Locative-specific: track entities within this context
        if not hasattr(model, '_locative_contents'):
            model._locative_contents = []
        
        logging.info(f"Applied locative case to model {model.name}")
        return model
    
    @staticmethod
    def process_update(model: Model, data: Any) -> Dict[str, Any]:
        """
        Process an update in the locative case.
        
        Locative updates involve managing context or containing
        other entities/operations.
        
        Args:
            model: The model being updated
            data: The update data
            
        Returns:
            Update result dictionary with context state
        """
        if hasattr(model, '_update_locative') and callable(getattr(model, '_update_locative')):
            return model._update_locative(data)
        
        result = {
            "status": "context",
            "role": "containing"
        }
        
        # Locative behavior: provide environmental context
        if hasattr(model, 'get_context') and callable(getattr(model, 'get_context')):
            result["context"] = model.get_context()
        
        if hasattr(model, '_locative_contents'):
            result["contents_count"] = len(model._locative_contents)
        
        return result
    
    @staticmethod
    def calculate_free_energy(model: Model) -> float:
        """
        Calculate free energy for locative case.
        
        In the locative case, free energy relates to the stability
        and coherence of the context being provided.
        
        Args:
            model: The model to calculate free energy for
            
        Returns:
            The calculated free energy
        """
        default_fe = 1.0
        
        # Locative: measure context stability
        if hasattr(model, 'context_stability'):
            try:
                stability = model.context_stability
                # Higher stability = lower free energy
                return (1.0 - stability) * LocativeCase.PRECISION
            except Exception as e:
                logging.warning(f"Error calculating locative free energy: {e}")
                return default_fe
        
        return default_fe
    
    @staticmethod
    def get_parameters(model: Model) -> Dict[str, Any]:
        """
        Get parameters relevant to locative case.
        
        Args:
            model: The model to get parameters from
            
        Returns:
            Dictionary of locative case parameters
        """
        params = {
            "context_type": model.parameters.get("context_type", "environment"),
            "max_capacity": model.parameters.get("max_capacity", 100),
            "isolation_level": model.parameters.get("isolation_level", "none"),
            "context_persistence": model.parameters.get("context_persistence", True)
        }
        
        return params
    
    @staticmethod
    def add_to_context(context: Model, entity: Any, 
                       metadata: Optional[Dict] = None) -> bool:
        """
        Add an entity to the locative context.
        
        Args:
            context: The locative context model
            entity: The entity to add
            metadata: Optional metadata about the entity
            
        Returns:
            True if entity was added successfully
        """
        if not hasattr(context, '_locative_contents'):
            context._locative_contents = []
        
        max_capacity = context.parameters.get("max_capacity", 100)
        if len(context._locative_contents) >= max_capacity:
            logging.warning(f"Context {context.name} at capacity")
            return False
        
        entry = {
            "entity": entity,
            "metadata": metadata or {}
        }
        context._locative_contents.append(entry)
        
        logging.info(f"Added entity to context {context.name}")
        return True
    
    @staticmethod
    def remove_from_context(context: Model, entity: Any) -> bool:
        """
        Remove an entity from the locative context.
        
        Args:
            context: The locative context model
            entity: The entity to remove
            
        Returns:
            True if entity was removed
        """
        if not hasattr(context, '_locative_contents'):
            return False
        
        initial_count = len(context._locative_contents)
        context._locative_contents = [
            e for e in context._locative_contents 
            if e.get("entity") != entity
        ]
        
        removed = len(context._locative_contents) < initial_count
        if removed:
            logging.info(f"Removed entity from context {context.name}")
        
        return removed
    
    @staticmethod
    def get_context_contents(context: Model) -> List[Any]:
        """
        Get all entities within the locative context.
        
        Args:
            context: The locative context model
            
        Returns:
            List of entities in the context
        """
        if not hasattr(context, '_locative_contents'):
            return []
        
        return [e.get("entity") for e in context._locative_contents]
    
    @staticmethod
    def can_transition_to(target_case: Case) -> bool:
        """
        Check if locative can transition to target case.
        
        Args:
            target_case: The target case to transition to
            
        Returns:
            True if transition is valid
        """
        return target_case in LocativeCase.VALID_TRANSITIONS
