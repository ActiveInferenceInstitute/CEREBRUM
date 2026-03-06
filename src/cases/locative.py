"""
Locative case implementation for CEREBRUM framework.

This module provides specialized methods for handling the locative case,
where a model serves as a context, container, or location for operations.
"""

from typing import Dict, Any, Optional, List
import logging

from ..core.model import Model, Case

logger = logging.getLogger(__name__)


class LocativeCase:
    """
    Locative case handler for models acting as contexts or containers.
    
    The locative case represents the model as an environment, context,
    or location in which other operations take place.
    """
    
    # Precision modifier for Active Inference
    PRECISION = 0.9  # Lower precision for context (more ambient)
    
    @staticmethod
    def apply(model: Model) -> Model:
        """Apply locative case to model, configuring it as a context or container."""
        model.case = Case.LOCATIVE
        
        # Locative-specific: track entities within this context
        if not hasattr(model, '_locative_contents'):
            model._locative_contents = []
        
        logger.info(f"Applied locative case to model {model.name}")
        return model
    
    @staticmethod
    def process_update(model: Model, data: Any) -> Dict[str, Any]:
        """Process a locative-case update, reporting environmental context and contained entities."""
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
        """Calculate free energy as precision-weighted instability: (1 - context_stability) * PRECISION."""
        default_fe = 1.0
        
        # Locative: measure context stability
        if hasattr(model, 'context_stability'):
            try:
                stability = model.context_stability
                # Higher stability = lower free energy
                return (1.0 - stability) * LocativeCase.PRECISION
            except (TypeError, ValueError) as e:
                logger.warning(f"Error calculating free energy: {e}")
                return default_fe
        
        return default_fe
    
    @staticmethod
    def get_parameters(model: Model) -> Dict[str, Any]:
        """Return locative-relevant parameters: context_type, max_capacity, isolation_level, context_persistence."""
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
        """Add entity to context's _locative_contents, respecting max_capacity. Returns False if at capacity."""
        if not hasattr(context, '_locative_contents'):
            context._locative_contents = []
        
        max_capacity = context.parameters.get("max_capacity", 100)
        if len(context._locative_contents) >= max_capacity:
            logger.warning(f"Context {context.name} at capacity")
            return False
        
        entry = {
            "entity": entity,
            "metadata": metadata or {}
        }
        context._locative_contents.append(entry)
        
        logger.info(f"Added entity to context {context.name}")
        return True
    
    @staticmethod
    def remove_from_context(context: Model, entity: Any) -> bool:
        """Remove entity from context's _locative_contents. Returns True if an entry was removed."""
        if not hasattr(context, '_locative_contents'):
            return False
        
        initial_count = len(context._locative_contents)
        context._locative_contents = [
            e for e in context._locative_contents 
            if e.get("entity") != entity
        ]
        
        removed = len(context._locative_contents) < initial_count
        if removed:
            logger.info(f"Removed entity from context {context.name}")
        
        return removed
    
    @staticmethod
    def get_context_contents(context: Model) -> List[Any]:
        """Return all entities currently held in context's _locative_contents."""
        if not hasattr(context, '_locative_contents'):
            return []
        
        return [e.get("entity") for e in context._locative_contents]
    
