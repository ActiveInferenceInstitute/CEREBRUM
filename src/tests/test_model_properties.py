"""
Property-based tests for model classes using hypothesis.

Property-based testing tests properties that should hold true for any valid input,
rather than specific examples. This helps find edge cases and unexpected behaviors.
"""
import pytest
import hypothesis
from hypothesis import given, strategies as st
import numpy as np
from src.core.model import Model, Case
from src.tests.test_utils import assert_allclose

# Mark these as property tests
pytestmark = [pytest.mark.unit, pytest.mark.model]

# Strategies for generating test inputs
model_names = st.text(min_size=1, max_size=20)
model_parameters = st.dictionaries(
    keys=st.text(min_size=1, max_size=10),
    values=st.one_of(
        st.integers(-100, 100),
        st.floats(min_value=-100.0, max_value=100.0),
        st.text(min_size=1, max_size=10)
    ),
    max_size=5
)
precision_values = st.floats(min_value=0.01, max_value=10.0)
case_values = st.sampled_from(list(Case))

@given(name=model_names, parameters=model_parameters)
def test_model_initialization_properties(name, parameters):
    """Test properties that should hold for any valid model initialization."""
    model = Model(name=name, parameters=parameters)
    
    # Property 1: A model's name should match what was provided
    assert model.name == name
    
    # Property 2: A model's parameters should match what was provided
    assert model.parameters == parameters
    
    # Property 3: Default case should always be NOMINATIVE
    assert model.case == Case.NOMINATIVE
    
    # Property 4: Initial case history should be empty
    assert model._case_history == []
    
    # Property 5: ID should be a valid UUID string
    assert len(model.id) == 36
    assert isinstance(model.id, str)

@given(
    name=model_names,
    case1=case_values,
    case2=case_values,
    case3=case_values
)
def test_case_transition_properties(name, case1, case2, case3):
    """Test properties of case transitions in models."""
    model = Model(name=name)
    
    # Record initial state
    original_case = model.case
    
    # Apply sequence of case transitions
    model.case = case1
    
    # Property 1: After setting case, it should match what was set
    assert model.case == case1
    
    # Property 2: If case1 != original, prior_case should be original
    if case1 != original_case:
        assert model._prior_case == original_case
        assert len(model._case_history) == 1
        assert model._case_history[0] == (original_case, case1)
    
    # Continue sequence
    model.case = case2
    
    # Property 3: Case history should accurately reflect transitions
    if case2 != case1:
        assert model._prior_case == case1
        # History length depends on whether first transition happened
        expected_history_len = 1 if case1 == original_case else 2
        assert len(model._case_history) == expected_history_len
        assert model._case_history[-1][0] == case1
        assert model._case_history[-1][1] == case2
    
    # Third transition
    model.case = case3
    
    # Property 4: Setting the same case twice should not add to history
    if case3 == case2:
        # Length should be unchanged from last assertion
        expected_history_len = 0  # Default for the case where no transitions occur
        
        # If there was at least one actual case change, calculate the expected history length
        if case1 != original_case:
            expected_history_len += 1
        if case2 != case1:
            expected_history_len += 1
            
        assert len(model._case_history) == expected_history_len
    
    # Property 5: ID and name should be unchanged after case transitions
    assert len(model.id) == 36
    assert model.name == name

@given(
    p1=precision_values,
    p2=precision_values,
    case1=case_values,
    case2=case_values
)
def test_precision_properties(p1, p2, case1, case2):
    """Test properties of precision settings in models."""
    # Skip if cases are the same to ensure distinct test cases
    if case1 == case2:
        hypothesis.assume(False)
    
    model = Model()
    
    # Set precision for two different cases
    model.set_precision(case1, p1)
    model.set_precision(case2, p2)
    
    # Property 1: Precision should match what was set for each case
    assert_allclose(model.get_precision(case1), p1)
    assert_allclose(model.get_precision(case2), p2)
    
    # Property 2: Getting precision without specifying case should return
    # precision for current case
    model.case = case1
    assert_allclose(model.get_precision(), p1)
    
    model.case = case2
    assert_allclose(model.get_precision(), p2)
    
    # Property 3: Setting a case's precision should not affect other cases
    for case in Case:
        if case != case1 and case != case2:
            assert_allclose(model.get_precision(case), 1.0)  # Default value 