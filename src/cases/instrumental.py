"""
Instrumental case implementation for CEREBRUM framework.

This module provides specialized methods for handling the instrumental case,
where a model serves as a tool or mechanism for performing operations.
"""

from typing import Dict, Any, Callable
import logging

from ..core.model import Model, Case

logger = logging.getLogger(__name__)


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
        """Apply instrumental case to model, configuring it as a tool or mechanism."""
        model.case = Case.INSTRUMENTAL
        
        # Instrumental-specific: ensure model has operation capabilities
        if not hasattr(model, '_instrument_operations'):
            model._instrument_operations = {}
        
        logger.info(f"Applied instrumental case to model {model.name}")
        return model
    
    @staticmethod
    def process_update(model: Model, data: Any) -> Dict[str, Any]:
        """Process an instrumental-case update, executing transformations on behalf of other models."""
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
        """Calculate free energy as precision-weighted absolute error between operation_cost and expected_cost."""
        default_fe = 1.0
        
        # Instrumental: measure operation efficiency
        if hasattr(model, 'operation_cost') and hasattr(model, 'expected_cost'):
            try:
                actual_cost = model.operation_cost
                expected_cost = model.expected_cost
                
                # Lower is better for tools
                efficiency_error = abs(actual_cost - expected_cost)
                return efficiency_error * InstrumentalCase.PRECISION
            except (AttributeError, TypeError, ValueError) as e:
                logger.warning(f"Error calculating free energy: {e}")
                return default_fe
        
        return default_fe
    
    @staticmethod
    def get_parameters(model: Model) -> Dict[str, Any]:
        """Return instrumental-relevant parameters: operation_mode, efficiency_threshold, max_operations, reusable."""
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
        """Invoke a named operation method on the model, returning None if the operation is absent."""
        if hasattr(model, operation) and callable(getattr(model, operation)):
            op_func = getattr(model, operation)
            result = op_func(input_data, **kwargs)
            logger.info(f"Executed operation '{operation}' via {model.name}")
            return result
        else:
            logger.warning(f"Operation '{operation}' not found on {model.name}")
            return None
    
    @staticmethod
    def register_operation(model: Model, name: str,
                          func: Callable) -> None:
        """Register func as a named operation on model, storing it in _instrument_operations and as an attribute."""
        if not hasattr(model, '_instrument_operations'):
            model._instrument_operations = {}
        
        model._instrument_operations[name] = func
        setattr(model, name, func)
        logger.info(f"Registered operation '{name}' on {model.name}")
