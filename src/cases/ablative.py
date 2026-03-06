"""
Ablative case implementation for CEREBRUM framework.

This module provides specialized methods for handling the ablative case,
where a model serves as an origin, source of motion, or starting point.
"""

from typing import Dict, Any, Optional
import numpy as np
import logging

from ..core.model import Model, Case

logger = logging.getLogger(__name__)


class AblativeCase:
    """
    Ablative case handler for models serving as origins or sources.
    
    The ablative case represents the model as a point of origin
    from which something moves away or is derived.
    """
    
    # Precision modifier for Active Inference
    PRECISION = 1.1  # Moderate precision for source/origin
    
    @staticmethod
    def apply(model: Model) -> Model:
        """Apply ablative case to model, designating it as an origin or starting point."""
        model.case = Case.ABLATIVE
        
        # Ablative-specific: track what has originated from this model
        if not hasattr(model, '_ablative_emissions'):
            model._ablative_emissions = []
        
        logger.info(f"Applied ablative case to model {model.name}")
        return model
    
    @staticmethod
    def process_update(model: Model, data: Any) -> Dict[str, Any]:
        """Process an ablative-case update, emitting state or data from this origin model."""
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
        """Calculate free energy as precision-weighted variance across recent _ablative_emissions."""
        default_fe = 1.0
        
        # Ablative: measure consistency of emissions
        if hasattr(model, '_ablative_emissions') and len(model._ablative_emissions) > 1:
            try:
                emissions = model._ablative_emissions[-10:]  # Last 10 emissions
                if all(isinstance(e, (int, float)) for e in emissions):
                    variance = np.var(emissions)
                    return variance * AblativeCase.PRECISION
            except (TypeError, ValueError) as e:
                logger.warning(f"Error calculating free energy: {e}")
                return default_fe
        
        return default_fe
    
    @staticmethod
    def get_parameters(model: Model) -> Dict[str, Any]:
        """Return ablative-relevant parameters: emission_rate, origin_tracking, max_emissions, emission_precision."""
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
        """Emit payload (defaulting to source state or parameters) from source to target, recording the emission."""
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
        
        logger.info(f"Emitted from {source.name} to {target.name}")
        return emission
    
