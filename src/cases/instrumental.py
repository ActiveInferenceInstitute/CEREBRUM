"""
Instrumental case implementation for CEREBRUM framework.

This module provides specialized methods for handling the instrumental case,
where a model serves as a tool or mechanism for performing operations.
"""

from typing import Dict, Any, Optional, List, Tuple, Union, Callable
import numpy as np
import logging

from ..core.model import Model, Case


class InstrumentalCase:
    """
    Instrumental case handler for models acting as tools or mechanisms.
    
    The instrumental case represents the model as a means by which
    an operation is performed - a tool, method, or mechanism.
    """
    
    # Precision modifier for Active Inference
    PRECISION = 0.8  # Lower precision for tools (more flexible)
    
    @staticmethod
    def apply(model: Model) -> Model:
        """
        Apply instrumental case configuration to a model.
        
        Args:
            model: The model to configure for instrumental case
            
        Returns:
            The configured model
        """
        model.case = Case.INSTRUMENTAL
        
        # Instrumental-specific: ensure model has operation capabilities
        if not hasattr(model, '_instrument_operations'):
            model._instrument_operations = {}
        
        logging.info(f"Applied instrumental case to model {model.name}")
        return model
    
    @staticmethod
    def process_update(model: Model, data: Any) -> Dict[str, Any]:
        """
        Process an update in the instrumental case.
        
        Instrumental updates involve performing operations or transformations
        on behalf of other models.
        
        Args:
            model: The model being updated
            data: The update data
            
        Returns:
            Update result dictionary with operation results
        """
        if hasattr(model, '_update_instrumental') and callable(getattr(model, '_update_instrumental')):
            return model._update_instrumental(data)
        
        result = {
            "status": "tool",
            "role": "operating"
        }
        
        # Instrumental behavior: perform transformation
        if hasattr(model, 'transform') and callable(getattr(model, 'transform')):
            result["transformation"] = model.transform(data)
        
        if hasattr(model, 'get_operation_log') and callable(getattr(model, 'get_operation_log')):
            result["operations"] = model.get_operation_log()
        
        return result
    
    @staticmethod
    def calculate_free_energy(model: Model) -> float:
        """
        Calculate free energy for instrumental case.
        
        In the instrumental case, free energy relates to the efficiency
        and accuracy of the tool's operation.
        
        Args:
            model: The model to calculate free energy for
            
        Returns:
            The calculated free energy
        """
        default_fe = 1.0
        
        # Instrumental: measure operation efficiency
        if hasattr(model, 'operation_cost') and hasattr(model, 'expected_cost'):
            try:
                actual_cost = model.operation_cost
                expected_cost = model.expected_cost
                
                # Lower is better for tools
                efficiency_error = abs(actual_cost - expected_cost)
                return efficiency_error * InstrumentalCase.PRECISION
            except Exception as e:
                logging.warning(f"Error calculating instrumental free energy: {e}")
                return default_fe
        
        return default_fe
    
    @staticmethod
    def get_parameters(model: Model) -> Dict[str, Any]:
        """
        Get parameters relevant to instrumental case.
        
        Args:
            model: The model to get parameters from
            
        Returns:
            Dictionary of instrumental case parameters
        """
        params = {
            "operation_mode": model.parameters.get("operation_mode", "transform"),
            "efficiency_threshold": model.parameters.get("efficiency_threshold", 0.9),
            "max_operations": model.parameters.get("max_operations", 100),
            "reusable": model.parameters.get("reusable", True)
        }
        
        return params
    
    @staticmethod
    def execute_operation(model: Model, operation: str, 
                         input_data: Any, **kwargs) -> Any:
        """
        Execute an operation using the instrumental model.
        
        Args:
            model: The instrumental model (tool)
            operation: Name of the operation to perform
            input_data: Data to operate on
            **kwargs: Additional operation parameters
            
        Returns:
            Result of the operation
        """
        if hasattr(model, operation) and callable(getattr(model, operation)):
            op_func = getattr(model, operation)
            result = op_func(input_data, **kwargs)
            logging.info(f"Executed operation '{operation}' via {model.name}")
            return result
        else:
            logging.warning(f"Operation '{operation}' not found on {model.name}")
            return None
    
    @staticmethod
    def register_operation(model: Model, name: str, 
                          func: Callable) -> None:
        """
        Register a new operation on an instrumental model.
        
        Args:
            model: The model to register the operation on
            name: Name of the operation
            func: The operation function
        """
        if not hasattr(model, '_instrument_operations'):
            model._instrument_operations = {}
        
        model._instrument_operations[name] = func
        setattr(model, name, func)
        logging.info(f"Registered operation '{name}' on {model.name}")
