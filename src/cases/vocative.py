"""
Vocative case implementation for CEREBRUM framework.

This module provides specialized methods for handling the vocative case,
where a model is directly addressed, invoked, or activated.
"""

from typing import Dict, Any, Optional, List, Tuple, Union, Callable
import numpy as np
import logging

from ..core.model import Model, Case


class VocativeCase:
    """
    Vocative case handler for models being directly addressed.
    
    The vocative case represents the model as the target of direct
    invocation, addressing, or activation.
    """
    
    # Precision modifier for Active Inference
    PRECISION = 2.0  # Highest precision for direct address (urgent attention)
    
    @staticmethod
    def apply(model: Model) -> Model:
        """
        Apply vocative case configuration to a model.
        
        Args:
            model: The model to configure for vocative case
            
        Returns:
            The configured model
        """
        model.case = Case.VOCATIVE
        
        # Vocative-specific: track invocations
        if not hasattr(model, '_vocative_invocations'):
            model._vocative_invocations = []
        
        logging.info(f"Applied vocative case to model {model.name}")
        return model
    
    @staticmethod
    def process_update(model: Model, data: Any) -> Dict[str, Any]:
        """
        Process an update in the vocative case.
        
        Vocative updates involve responding to direct address or invocation.
        
        Args:
            model: The model being updated
            data: The update data
            
        Returns:
            Update result dictionary with response
        """
        if hasattr(model, '_update_vocative') and callable(getattr(model, '_update_vocative')):
            return model._update_vocative(data)
        
        result = {
            "status": "addressed",
            "role": "responding",
            "attention": "high"
        }
        
        # Record invocation
        if hasattr(model, '_vocative_invocations'):
            model._vocative_invocations.append({
                "data": data,
                "timestamp": None  # Would use datetime in production
            })
        
        # Vocative behavior: immediate response
        if hasattr(model, 'respond') and callable(getattr(model, 'respond')):
            result["response"] = model.respond(data)
        
        return result
    
    @staticmethod
    def calculate_free_energy(model: Model) -> float:
        """
        Calculate free energy for vocative case.
        
        In the vocative case, free energy relates to the quality
        and timeliness of response to direct addressing.
        
        Args:
            model: The model to calculate free energy for
            
        Returns:
            The calculated free energy
        """
        default_fe = 1.0
        
        # Vocative: highest precision means errors are costly
        if hasattr(model, 'response_latency'):
            try:
                latency = model.response_latency
                # High latency = high free energy (bad for vocative)
                return latency * VocativeCase.PRECISION
            except Exception as e:
                logging.warning(f"Error calculating vocative free energy: {e}")
                return default_fe
        
        return default_fe
    
    @staticmethod
    def get_parameters(model: Model) -> Dict[str, Any]:
        """
        Get parameters relevant to vocative case.
        
        Args:
            model: The model to get parameters from
            
        Returns:
            Dictionary of vocative case parameters
        """
        params = {
            "response_timeout": model.parameters.get("response_timeout", 1.0),
            "priority_level": model.parameters.get("priority_level", "high"),
            "acknowledgment_required": model.parameters.get("acknowledgment_required", True),
            "attention_threshold": model.parameters.get("attention_threshold", 0.8)
        }
        
        return params
    
    @staticmethod
    def invoke(model: Model, invocation: Dict[str, Any]) -> Dict[str, Any]:
        """
        Directly invoke a model in vocative case.
        
        Args:
            model: The model to invoke
            invocation: The invocation data
            
        Returns:
            Response from the model
        """
        # Record the invocation
        if hasattr(model, '_vocative_invocations'):
            model._vocative_invocations.append(invocation)
        
        response = {
            "acknowledged": True,
            "model": model.name,
            "precision": VocativeCase.PRECISION
        }
        
        # Attempt to get immediate response
        if hasattr(model, 'respond') and callable(getattr(model, 'respond')):
            response["content"] = model.respond(invocation)
        elif hasattr(model, 'process') and callable(getattr(model, 'process')):
            response["content"] = model.process(invocation)
        else:
            response["content"] = {"status": "acknowledged"}
        
        logging.info(f"Invoked model {model.name} in vocative case")
        return response
    
    @staticmethod
    def broadcast(models: List[Model], message: Any) -> List[Dict[str, Any]]:
        """
        Broadcast a message to multiple models (multi-vocative).
        
        Args:
            models: List of models to address
            message: The message to broadcast
            
        Returns:
            List of responses from all models
        """
        responses = []
        
        for model in models:
            invocation = {
                "message": message,
                "broadcast": True
            }
            response = VocativeCase.invoke(model, invocation)
            responses.append(response)
        
        logging.info(f"Broadcast to {len(models)} models")
        return responses
    
    @staticmethod
    def get_invocation_history(model: Model, limit: int = 10) -> List[Dict]:
        """
        Get recent invocation history for a model.
        
        Args:
            model: The model to get history for
            limit: Maximum number of invocations to return
            
        Returns:
            List of recent invocations
        """
        if not hasattr(model, '_vocative_invocations'):
            return []
        
        return model._vocative_invocations[-limit:]
