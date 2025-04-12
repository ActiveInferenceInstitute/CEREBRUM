"""
Integration tests for model interactions and message passing between models.
"""
import pytest
import numpy as np
from src.core.model import Model, Case
from src.core.active_inference import ActiveInferenceModel

# Mark as integration tests
pytestmark = [pytest.mark.integration]

class SimpleActiveInferenceModel(ActiveInferenceModel):
    """A simplified ActiveInferenceModel for testing interactions."""
    
    def __init__(self, name=None, parameters=None, n_states=3, n_observations=3):
        prior_means = np.zeros(n_states)
        prior_precision = np.eye(n_states)
        likelihood_precision = np.eye(n_observations)
        
        super().__init__(
            name=name,
            parameters=parameters,
            prior_means=prior_means,
            prior_precision=prior_precision,
            likelihood_precision=likelihood_precision
        )
        
        # Override abstract methods for testing
        self._has_received_data = False
    
    def likelihood(self, states):
        """Simple linear likelihood function."""
        return states
    
    def _update_nominative(self, data):
        """Override nominative update to track calls."""
        result = super()._update_nominative(data)
        self._has_received_data = data is not None
        result["custom"] = "nominative_called"
        return result
    
    def _update_accusative(self, data):
        """Override accusative update to track calls."""
        result = super()._update_accusative(data)
        self._has_received_data = data is not None
        result["custom"] = "accusative_called"
        return result

@pytest.fixture
def connected_models():
    """Create a set of connected models for testing interactions."""
    model1 = SimpleActiveInferenceModel(name="Model1")
    model2 = SimpleActiveInferenceModel(name="Model2")
    model3 = SimpleActiveInferenceModel(name="Model3")
    
    # Create a simple network: Model1 -> Model2 -> Model3
    model1.connect(model2, relation_type="feedforward")
    model2.connect(model3, relation_type="feedforward")
    model3.connect(model1, relation_type="feedback")
    
    return model1, model2, model3

class TestModelInteractions:
    """Integration tests for model interactions."""
    
    def test_message_passing(self, connected_models):
        """Test message passing between connected models."""
        model1, model2, model3 = connected_models
        
        # Set up test data
        test_data = np.array([1.0, 2.0, 3.0])
        
        # Model1 generates predictions as NOMINATIVE
        result1 = model1.update(test_data)
        assert result1["status"] == "success"
        assert result1["custom"] == "nominative_called"
        
        # Model1 sends predictions to Model2 (acting as DATIVE recipient)
        model2.case = Case.DATIVE
        predictions = result1["predictions"]
        
        result2 = model2.update(predictions)
        assert model2._has_received_data
        
        # Model2 processes and becomes NOMINATIVE to generate its own predictions
        model2.case = Case.NOMINATIVE
        result2 = model2.update()
        assert result2["status"] == "success"
        
        # Model2 sends predictions to Model3
        model3.case = Case.ACCUSATIVE
        predictions = result2["predictions"]
        
        result3 = model3.update(predictions)
        assert result3["status"] == "success"
        assert result3["custom"] == "accusative_called"
        assert model3._has_received_data
    
    def test_precision_weighting(self, connected_models):
        """Test precision weighting affects message passing."""
        model1, model2, model3 = connected_models
        
        # Set different precision weights
        model1.set_precision(Case.NOMINATIVE, 0.5)
        model2.set_precision(Case.DATIVE, 2.0)
        
        # Model1 generates predictions as NOMINATIVE
        test_data = np.array([1.0, 2.0, 3.0])
        result1 = model1.update(test_data)
        
        # Lower precision in model1 should affect prediction certainty
        assert model1.get_precision() == 0.5
        
        # Model2 receives with higher precision
        model2.case = Case.DATIVE
        assert model2.get_precision() == 2.0
        
        # The update should still succeed with different precision weighting
        predictions = result1["predictions"]
        result2 = model2.update(predictions)
        assert result2["status"] == "success"
    
    def test_case_changes_during_interaction(self, connected_models):
        """Test that case changes affect how models interact."""
        model1, model2, model3 = connected_models
        
        # Initial cases
        assert model1.case == Case.NOMINATIVE
        assert model2.case == Case.NOMINATIVE
        assert model3.case == Case.NOMINATIVE
        
        # Change Model2 to INSTRUMENTAL (tool/method)
        model2.case = Case.INSTRUMENTAL
        assert model2.case == Case.INSTRUMENTAL
        assert model2._prior_case == Case.NOMINATIVE
        
        # Model1 can use Model2 as a tool
        test_data = np.array([1.0, 2.0, 3.0])
        result1 = model1.update(test_data)
        
        # Model1 and Model3 should still be NOMINATIVE
        assert model1.case == Case.NOMINATIVE
        assert model3.case == Case.NOMINATIVE
        
        # Model2 remains INSTRUMENTAL
        assert model2.case == Case.INSTRUMENTAL 