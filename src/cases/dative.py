"""
Dative case implementation for CEREBRUM framework.

This module provides specialized methods for handling the dative case,
where a model acts as a recipient of data flows.
"""

from typing import Dict, Any, Optional, List, Tuple, Union, Callable
import numpy as np
import logging

from ..core.model import Model, Case

class DativeCase:
    """
    Dative case handler for models acting as recipients.
    
    The dative case represents the model as a recipient of data flows,
    receiving information from other models or external sources.
    """
    
    @staticmethod
    def apply(model: Model) -> Model:
        """
        Apply dative case configuration to a model.
        
        Args:
            model: The model to configure for dative case
            
        Returns:
            The configured model
        """
        # Set the case through the model's case property
        model.case = Case.DATIVE
        
        # Additional dative-specific initializations
        # For example, ensure data reception buffers are enabled
        
        logging.info(f"Applied dative case to model {model.name}")
        return model
    
    @staticmethod
    def process_update(model: Model, data: Any) -> Dict[str, Any]:
        """
        Process an update in the dative case.
        
        This handles receipt of data from external sources.
        
        Args:
            model: The model being updated
            data: The update data
            
        Returns:
            Update result dictionary
        """
        # Call the model's internal dative update method if it exists
        if hasattr(model, '_update_dative') and callable(getattr(model, '_update_dative')):
            return model._update_dative(data)
        
        # Default implementation for models without their own method
        result = {
            "status": "received",
            "data_processed": []
        }
        
        # Handle different types of data receipt
        if isinstance(data, dict):
            # Process different keys in the data dictionary
            if "goal" in data or "goal_position" in data:
                # Update goal if provided
                goal = data.get("goal", data.get("goal_position"))
                if hasattr(model, "goal_position"):
                    model.goal_position = np.array(goal)
                elif hasattr(model, "goal"):
                    model.goal = goal
                result["data_processed"].append("goal")
                logging.info(f"Model {model.name} received new goal: {goal}")
            
            if "sensory_data" in data:
                # Process sensory data if provided
                sensory = data["sensory_data"]
                if hasattr(model, "sensory"):
                    model.sensory = sensory
                result["data_processed"].append("sensory_data")
            
            if "message" in data:
                # Process message data if provided
                message = data["message"]
                if hasattr(model, "messages"):
                    if isinstance(model.messages, list):
                        model.messages.append(message)
                    else:
                        model.messages = [message]
                result["data_processed"].append("message")
        
        return result
    
    @staticmethod
    def calculate_free_energy(model: Model) -> float:
        """
        Calculate free energy for dative case.
        
        In the dative case, free energy often relates to the divergence
        between expected and actual received data.
        
        Args:
            model: The model to calculate free energy for
            
        Returns:
            The calculated free energy
        """
        # Default implementation - models should override this
        default_fe = 0.3
        
        # If the model has expected and received data, calculate divergence
        if hasattr(model, 'expected_data') and hasattr(model, 'received_data'):
            try:
                expected = getattr(model, 'expected_data')
                received = getattr(model, 'received_data')
                
                # Simple Euclidean distance if data are array-like
                if hasattr(expected, 'shape') and hasattr(received, 'shape'):
                    return np.linalg.norm(received - expected)
                # Otherwise, return a binary match/no-match
                else:
                    return 0.0 if expected == received else 0.3
            except Exception as e:
                logging.warning(f"Error calculating dative free energy: {e}")
                return default_fe
                
        return default_fe
    
    @staticmethod
    def receive_data(model: Model, data: Any, data_type: str = "general") -> Dict[str, Any]:
        """
        Process incoming data to a model in the dative case.
        
        Args:
            model: The model receiving data
            data: The data being received
            data_type: Type of data being received
            
        Returns:
            Dictionary with receiving results
        """
        result = {
            "status": "received",
            "data_type": data_type
        }
        
        # Handle different data types
        if data_type == "goal":
            if hasattr(model, "goal_position"):
                model.goal_position = np.array(data)
                result["details"] = f"Updated goal position to {data}"
                
        elif data_type == "perception":
            if hasattr(model, "vision_field"):
                model.vision_field = np.array(data)
                result["details"] = "Updated vision field"
                
        elif data_type == "command":
            # Store the command for later processing
            if not hasattr(model, "_pending_commands"):
                model._pending_commands = []
            model._pending_commands.append(data)
            result["details"] = f"Stored command: {data}"
        
        else:
            # Generic data storage
            if not hasattr(model, "_received_data"):
                model._received_data = {}
            model._received_data[data_type] = data
            result["details"] = f"Stored {data_type} data"
        
        logging.info(f"Model {model.name} received {data_type} data")
        return result 