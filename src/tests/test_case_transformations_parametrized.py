"""
Parameterized tests for case transformations using pytest fixtures and test parameterization.
"""
import pytest
import numpy as np
from typing import Dict, Any, Tuple
from src.core.model import Case, Model
from src.transformations.case_transformations import (
    apply_nominative_transformation,
    apply_accusative_transformation,
    apply_genitive_transformation,
    apply_dative_transformation,
    apply_instrumental_transformation,
    apply_locative_transformation,
    apply_ablative_transformation,
    apply_vocative_transformation
)
from src.tests.test_utils import assert_allclose

# Mark these as unit tests and transformation tests
pytestmark = [pytest.mark.unit, pytest.mark.transformation]

# Sample model data for tests
@pytest.fixture
def base_model_data():
    """Provide a consistent model for transformation tests."""
    class TestModel(Model):
        def __init__(self, name="TestModel", parameters=None):
            super().__init__(name=name, parameters=parameters or {})
            # Add test-specific attributes
            self.attention_weights = np.ones(3)
            self.connection_strengths = np.array([0.5, 0.3, 0.8])
            self.value = 10
    
    return TestModel()

# Transformation function mapping
TRANSFORMATION_MAP = {
    Case.NOMINATIVE: apply_nominative_transformation,
    Case.ACCUSATIVE: apply_accusative_transformation,
    Case.GENITIVE: apply_genitive_transformation,
    Case.DATIVE: apply_dative_transformation,
    Case.INSTRUMENTAL: apply_instrumental_transformation,
    Case.LOCATIVE: apply_locative_transformation,
    Case.ABLATIVE: apply_ablative_transformation,
    Case.VOCATIVE: apply_vocative_transformation
}

# Expected transformations for each case
@pytest.fixture
def case_expectations():
    """Define the expected transformations for each case."""
    return {
        Case.NOMINATIVE: {
            'attention_weights': np.ones(3),  # No change
            'connection_strengths': np.array([0.5, 0.3, 0.8]),  # No change
            'value': 10  # No change
        },
        Case.ACCUSATIVE: {
            'attention_weights': np.ones(3) * 0.8,  # Reduced attention
            'connection_strengths': np.array([0.5, 0.3, 0.8]) * 1.2,  # Increased connectivity
            'value': 12  # Value increased by 20%
        },
        Case.GENITIVE: {
            'attention_weights': np.ones(3) * 1.5,  # Increased source attention
            'connection_strengths': np.array([0.5, 0.3, 0.8]) * 0.7,  # Reduced output connectivity
            'value': 7  # Value decreased
        },
        # Other cases defined similarly...
    }

# Parameterized test for all case transformations
@pytest.mark.parametrize('target_case', list(Case))
def test_case_transformation(base_model_data, case_expectations, target_case):
    """
    Test that each case transformation correctly modifies the model.
    
    Args:
        base_model_data: Fixture providing a test model
        case_expectations: Fixture with expected values after transformation
        target_case: The case to transform the model into
    """
    # Skip cases not defined in expectations
    if target_case not in case_expectations:
        pytest.skip(f"No expectations defined for {target_case}")
    
    # Get the transformation function
    transform_func = TRANSFORMATION_MAP[target_case]
    
    # Apply the transformation
    model = base_model_data
    transform_func(model)
    
    # Check expected values
    expectations = case_expectations[target_case]
    
    # Check numerical attributes with tolerance
    if 'attention_weights' in expectations:
        assert_allclose(
            model.attention_weights, 
            expectations['attention_weights'],
            err_msg=f"attention_weights incorrect for {target_case}"
        )
    
    if 'connection_strengths' in expectations:
        assert_allclose(
            model.connection_strengths, 
            expectations['connection_strengths'],
            err_msg=f"connection_strengths incorrect for {target_case}"
        )
    
    if 'value' in expectations:
        assert_allclose(
            model.value, 
            expectations['value'],
            err_msg=f"value incorrect for {target_case}"
        )

# Test transformation sequence
def test_sequential_transformations(base_model_data):
    """Test that sequential transformations work correctly."""
    model = base_model_data
    original_value = model.value
    
    # Apply sequence: NOMINATIVE -> ACCUSATIVE -> GENITIVE -> NOMINATIVE
    apply_accusative_transformation(model)
    assert model.value != original_value, "Value should change after ACCUSATIVE transformation"
    
    interim_value = model.value
    apply_genitive_transformation(model)
    assert model.value != interim_value, "Value should change after GENITIVE transformation"
    
    apply_nominative_transformation(model)
    # Check if transformation chain returns to original
    # Note: This might not always be true depending on the implementation,
    # but it's a good test of the transformation logic
    assert_allclose(model.value, original_value, rtol=1e-5) 