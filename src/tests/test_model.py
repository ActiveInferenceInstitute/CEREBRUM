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
    def test_model_update_dispatch_abstract(self, case_to_test):
        """Test that update dispatches correctly and case methods raise NotImplementedError."""
        model = Model()
        model.case = case_to_test
        with pytest.raises(NotImplementedError):
            model.update(data="test_data")
    
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