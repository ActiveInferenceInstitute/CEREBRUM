"""
Parameterized tests for case transformations using pytest fixtures and test parameterization.
"""
import pytest
import numpy as np
from typing import Dict, Any, Tuple
from src.core.model import Case, Model
from src.transformations.case_transformations import (
    transform_case,
    revert_case,
    apply_morphosyntactic_alignment,
    create_case_relationship
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

# Parameterized test for case transformations
@pytest.mark.parametrize('target_case', list(Case))
def test_transform_case(base_model_data, target_case):
    """
    Test that a model can be transformed to each case.
    
    Args:
        base_model_data: Fixture providing a test model
        target_case: The case to transform the model into
    """
    # Apply the transformation
    model = base_model_data
    original_case = model.case
    
    # Transform to target case
    result = transform_case(model, target_case)
    
    # Verify the model was transformed
    assert result is model  # Should return the same instance
    assert model.case == target_case
    
    # If we changed case, prior_case should be set
    if original_case != target_case:
        assert model._prior_case == original_case
        assert (original_case, target_case) in model._case_history

def test_revert_case(base_model_data):
    """Test that a model can be reverted to its previous case."""
    model = base_model_data
    original_case = model.case
    
    # First transform to a different case
    transform_case(model, Case.ACCUSATIVE)
    assert model.case == Case.ACCUSATIVE
    
    # Now revert to the previous case
    revert_case(model)
    assert model.case == original_case
    assert model._case_history[-1] == (Case.ACCUSATIVE, original_case)

def test_apply_morphosyntactic_alignment():
    """Test applying morphosyntactic alignment to a group of models."""
    # Create a set of test models
    models = [
        Model(name=f"Model_{i}") 
        for i in range(3)
    ]
    
    # Apply nominative-accusative alignment
    subject = models[0]
    object_model = models[1]
    
    result = apply_morphosyntactic_alignment(
        models,
        alignment_type="nominative_accusative",
        subject=subject,
        object=object_model
    )
    
    # Check the subject got nominative case
    assert subject.case == Case.NOMINATIVE
    assert subject in result["NOMINATIVE"]
    
    # Check the object got accusative case
    assert object_model.case == Case.ACCUSATIVE
    assert object_model in result["ACCUSATIVE"]

def test_create_case_relationship():
    """Test creating a case relationship between two models."""
    source = Model(name="SourceModel")
    target = Model(name="TargetModel")
    
    # Test "generates" relationship
    source_result, target_result = create_case_relationship(
        source, target, "generates"
    )
    
    assert source_result is source
    assert target_result is target
    assert source.case == Case.NOMINATIVE
    assert target.case == Case.DATIVE
    
    # Reset cases
    source.case = Case.NOMINATIVE
    target.case = Case.NOMINATIVE
    
    # Test another relationship type
    create_case_relationship(source, target, "updates")
    assert source.case == Case.NOMINATIVE  # Updates uses NOMINATIVE for source
    # The rest of the test would depend on implementation 