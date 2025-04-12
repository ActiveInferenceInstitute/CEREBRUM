"""
Test utility functions and classes to support testing in the CEREBRUM project.
"""
import numpy as np
import tempfile
import os
from pathlib import Path
from typing import Dict, Any, List, Tuple, Optional, Union

# Model comparison utilities
def assert_models_equal(model1, model2, check_attributes: List[str] = None):
    """
    Assert that two models are equal by checking their attributes.
    
    Args:
        model1: First model
        model2: Second model
        check_attributes: List of attribute names to check, defaults to key attributes
    """
    if check_attributes is None:
        check_attributes = ['name', 'parameters', 'case']
    
    for attr in check_attributes:
        assert hasattr(model1, attr), f"Model 1 missing attribute: {attr}"
        assert hasattr(model2, attr), f"Model 2 missing attribute: {attr}"
        
        attr1 = getattr(model1, attr)
        attr2 = getattr(model2, attr)
        
        if isinstance(attr1, np.ndarray) and isinstance(attr2, np.ndarray):
            assert np.array_equal(attr1, attr2), f"Arrays not equal for attribute {attr}"
        else:
            assert attr1 == attr2, f"Values not equal for attribute {attr}: {attr1} != {attr2}"

# Numerical test helpers
def assert_allclose(actual, expected, rtol=1e-5, atol=1e-8, err_msg=''):
    """
    Assert that arrays or values are almost equal (wrapper around np.testing.assert_allclose).
    
    Args:
        actual: Array or value to test
        expected: Expected array or value
        rtol: Relative tolerance
        atol: Absolute tolerance
        err_msg: Error message to display
    """
    np.testing.assert_allclose(actual, expected, rtol=rtol, atol=atol, err_msg=err_msg)

def generate_test_data(shape=(10, 3), mean=0, std=1, seed=None):
    """
    Generate random test data for use in tests.
    
    Args:
        shape: Shape of the test data array
        mean: Mean of the normal distribution
        std: Standard deviation of the normal distribution
        seed: Random seed for reproducibility
        
    Returns:
        np.ndarray: Generated test data
    """
    if seed is not None:
        np.random.seed(seed)
    return np.random.normal(mean, std, size=shape)

# File and IO helpers
class TempFileManager:
    """Context manager for creating and cleaning up temporary files during tests."""
    
    def __init__(self, suffix='.tmp', dir=None):
        """
        Initialize the temporary file manager.
        
        Args:
            suffix: File suffix
            dir: Directory for temporary files
        """
        self.suffix = suffix
        self.dir = dir
        self.files = []
    
    def __enter__(self):
        return self
    
    def create_file(self, content='', suffix=None):
        """
        Create a temporary file with the given content.
        
        Args:
            content: String content to write to the file
            suffix: Optional suffix override
            
        Returns:
            Path to the created temporary file
        """
        suffix = suffix or self.suffix
        fd, path = tempfile.mkstemp(suffix=suffix, dir=self.dir)
        os.close(fd)
        
        with open(path, 'w') as f:
            f.write(content)
        
        self.files.append(path)
        return path
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Clean up temporary files on context manager exit."""
        for file_path in self.files:
            try:
                if os.path.exists(file_path):
                    os.unlink(file_path)
            except Exception as e:
                print(f"Error cleaning up temporary file {file_path}: {e}")

# Mocking helpers
class MockModel:
    """Mock Model class for testing components that depend on models."""
    
    def __init__(self, name="MockModel", parameters=None, case=None):
        """Initialize a mock model with the given parameters."""
        from src.core.model import Case
        self.id = "mock-id-123"
        self.name = name
        self.parameters = parameters or {}
        self._case = case or Case.NOMINATIVE
        self._prior_case = None
        self._case_history = []
        self.connections = []
        self._precision_weights = {case: 1.0 for case in Case}
    
    @property
    def case(self):
        """Get the current case."""
        return self._case
    
    @case.setter
    def case(self, new_case):
        """Set the current case."""
        if new_case != self._case:
            self._prior_case = self._case
            self._case = new_case
            self._case_history.append((self._prior_case, new_case))
    
    def get_precision(self, case=None):
        """Get the precision for the given case."""
        if case is None:
            case = self._case
        return self._precision_weights[case]
    
    def free_energy(self):
        """Mock free energy calculation."""
        return 0.0
    
    def update(self, data=None):
        """Mock update method."""
        return {"status": "success", "mock": True} 