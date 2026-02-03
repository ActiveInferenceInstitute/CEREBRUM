import pytest
import uuid
import numpy as np
from src.core.model import Model, Case
from src.tests.test_utils import assert_models_equal

# Mark these as unit tests and model tests
pytestmark = [pytest.mark.unit, pytest.mark.model]

class TestCase:
    """Tests for the Case enumeration"""
    
    def test_case_enum(self):
        """Test the Case enum members and values."""
        assert Case.NOMINATIVE.value == "NOM"
        assert Case.ACCUSATIVE.value == "ACC"
        assert Case.GENITIVE.value == "GEN"
        assert Case.DATIVE.value == "DAT"
        assert Case.INSTRUMENTAL.value == "INS"
        assert Case.LOCATIVE.value == "LOC"
        assert Case.ABLATIVE.value == "ABL"
        assert Case.VOCATIVE.value == "VOC"
        assert len(Case) == 8
    
    def test_case_equality(self):
        """Test that Case instances can be properly compared."""
        assert Case.NOMINATIVE == Case.NOMINATIVE
        assert Case.NOMINATIVE != Case.ACCUSATIVE
        assert Case.ACCUSATIVE != Case.GENITIVE
        assert Case.DATIVE is Case.DATIVE

class TestModel:
    """Tests for the base Model class"""
    
    def test_model_init_defaults(self):
        """Test Model initialization with default values."""
        model = Model()
        assert isinstance(model.id, str)
        assert len(model.id) == 36  # Standard UUID length
        assert model.name.startswith("Model_")
        assert model.parameters == {}
        assert model.case == Case.NOMINATIVE
        assert model._prior_case is None
        assert model._case_history == []
        assert len(model._case_configurations) == len(Case)
        assert len(model._precision_weights) == len(Case)
        for case in Case:
            assert model.get_precision(case) == 1.0
        assert model.connections == []
    
    def test_model_init_custom(self, default_model_params):
        """Test Model initialization with custom name and parameters."""
        model = Model(name="TestModel", parameters=default_model_params)
        assert model.name == "TestModel"
        assert model.parameters == default_model_params
        # Other defaults should still hold
        assert model.case == Case.NOMINATIVE
        assert model.get_precision() == 1.0
    
    def test_model_case_setter(self):
        """Test setting the model's case."""
        model = Model()
        initial_case = model.case
        assert initial_case == Case.NOMINATIVE
        
        model.case = Case.ACCUSATIVE
        assert model.case == Case.ACCUSATIVE
        assert model._prior_case == Case.NOMINATIVE
        assert model._case_history == [(Case.NOMINATIVE, Case.ACCUSATIVE)]
        
        # Set back to original
        model.case = Case.NOMINATIVE
        assert model.case == Case.NOMINATIVE
        assert model._prior_case == Case.ACCUSATIVE
        assert model._case_history == [
            (Case.NOMINATIVE, Case.ACCUSATIVE),
            (Case.ACCUSATIVE, Case.NOMINATIVE)
        ]
        
        # Setting the same case should do nothing
        history_len = len(model._case_history)
        model.case = Case.NOMINATIVE
        assert model.case == Case.NOMINATIVE
        assert model._prior_case == Case.ACCUSATIVE  # Should not change
        assert len(model._case_history) == history_len  # Should not change
    
    def test_model_precision(self):
        """Test setting and getting precision weights."""
        model = Model()
        assert model.get_precision() == 1.0  # Default for current case (NOMINATIVE)
        assert model.get_precision(Case.GENITIVE) == 1.0  # Default for specific case
        
        model.set_precision(Case.GENITIVE, 0.5)
        assert model.get_precision(Case.GENITIVE) == 0.5
        assert model.get_precision() == 1.0  # Current case precision unchanged
        
        model.case = Case.GENITIVE
        assert model.get_precision() == 0.5  # Now current case precision is 0.5
    
    def test_model_connect(self):
        """Test connecting models."""
        model1 = Model(name="Model1")
        model2 = Model(name="Model2")
        
        assert model1.connections == []
        model1.connect(model2, relation_type="dependency")
        assert len(model1.connections) == 1
        connected_model, relation = model1.connections[0]
        assert connected_model == model2
        assert relation == "dependency"
        
        # Connecting again
        model1.connect(model2, relation_type="feedback")
        assert len(model1.connections) == 2
        assert model1.connections[1] == (model2, "feedback")
    
    def test_model_free_energy_abstract(self):
        """Test that free_energy raises NotImplementedError in the base class."""
        model = Model()
        with pytest.raises(NotImplementedError):
            model.free_energy()
    
    @pytest.mark.parametrize("case_to_test", list(Case))
    def test_model_update_dispatch_default(self, case_to_test):
        """Test that update dispatches correctly and returns default success responses."""
        model = Model()
        model.case = case_to_test
        result = model.update(data="test_data")
        # Base Model now provides default implementations
        assert result["status"] == "success"
        # Case names are lowercase in default response (nominative, accusative, etc.)
        assert result["case"] == case_to_test.name.lower()
        assert result["data_received"] is True
    
    def test_model_repr(self):
        """Test the __repr__ method."""
        model = Model(name="MyModel")
        assert repr(model) == "MyModel[NOM]"
        model.case = Case.INSTRUMENTAL
        assert repr(model) == "MyModel[INS]"
    
    def test_model_equality_comparison(self):
        """Test that two models with the same attributes are considered equal."""
        params = {"param1": 10, "param2": "value"}
        model1 = Model(name="TestModel", parameters=params)
        model2 = Model(name="TestModel", parameters=params)
        
        # Different IDs but same functional properties
        assert model1.id != model2.id
        
        # Test our custom equality check from test_utils
        assert_models_equal(model1, model2)
    
    def test_model_get_state(self):
        """Test the get_state method returns correct state dictionary."""
        params = {"learning_rate": 0.01}
        model = Model(name="StateTestModel", parameters=params)
        state = model.get_state()
        
        assert state["id"] == model.id
        assert state["name"] == "StateTestModel"
        assert state["case"] == "NOM"
        assert state["prior_case"] is None
        assert state["case_history_length"] == 0
        assert state["num_connections"] == 0
        assert state["precision"] == 1.0
        assert state["parameters"] == params
        
        # After case change
        model.case = Case.DATIVE
        state = model.get_state()
        assert state["case"] == "DAT"
        assert state["prior_case"] == "NOM"
        assert state["case_history_length"] == 1
    
    def test_model_set_parameters(self):
        """Test the set_parameters method updates parameters correctly."""
        model = Model(name="ParamModel", parameters={"a": 1})
        assert model.parameters == {"a": 1}
        
        model.set_parameters({"b": 2, "c": 3})
        assert model.parameters == {"a": 1, "b": 2, "c": 3}
        
        model.set_parameters({"a": 10})  # Override existing
        assert model.parameters["a"] == 10
    
    def test_model_get_case_history(self):
        """Test the get_case_history method returns history correctly."""
        model = Model()
        assert model.get_case_history() == []
        
        model.case = Case.ACCUSATIVE
        history = model.get_case_history()
        assert len(history) == 1
        assert history[0] == (Case.NOMINATIVE, Case.ACCUSATIVE)
        
        model.case = Case.GENITIVE
        history = model.get_case_history()
        assert len(history) == 2
        assert history[1] == (Case.ACCUSATIVE, Case.GENITIVE)
        
        # Verify it returns a copy
        returned_history = model.get_case_history()
        returned_history.append(("test", "test"))
        assert len(model.get_case_history()) == 2
    
    def test_model_predict_abstract(self):
        """Test that predict raises NotImplementedError in base class."""
        model = Model()
        with pytest.raises(NotImplementedError):
            model.predict(data="test") 