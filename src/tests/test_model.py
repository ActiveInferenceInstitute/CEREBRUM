import pytest
import uuid
from src.core.model import Model, Case

# Test Case Enum
def test_case_enum():
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

# Test Model Initialization
def test_model_init_defaults():
    """Test Model initialization with default values."""
    model = Model()
    assert isinstance(model.id, str)
    assert len(model.id) == 36 # Standard UUID length
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

def test_model_init_custom():
    """Test Model initialization with custom name and parameters."""
    params = {"param1": 10, "param2": "value"}
    model = Model(name="TestModel", parameters=params)
    assert model.name == "TestModel"
    assert model.parameters == params
    # Other defaults should still hold
    assert model.case == Case.NOMINATIVE
    assert model.get_precision() == 1.0

# Test Case Property and Setter
def test_model_case_setter():
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
    assert model._prior_case == Case.ACCUSATIVE # Should not change
    assert len(model._case_history) == history_len # Should not change

# Test Precision Methods
def test_model_precision():
    """Test setting and getting precision weights."""
    model = Model()
    assert model.get_precision() == 1.0 # Default for current case (NOMINATIVE)
    assert model.get_precision(Case.GENITIVE) == 1.0 # Default for specific case

    model.set_precision(Case.GENITIVE, 0.5)
    assert model.get_precision(Case.GENITIVE) == 0.5
    assert model.get_precision() == 1.0 # Current case precision unchanged

    model.case = Case.GENITIVE
    assert model.get_precision() == 0.5 # Now current case precision is 0.5

# Test Connections
def test_model_connect():
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

# Test Abstract Methods
def test_model_free_energy_abstract():
    """Test that free_energy raises NotImplementedError in the base class."""
    model = Model()
    with pytest.raises(NotImplementedError):
        model.free_energy()

# Parameterize tests for all case-specific update methods
@pytest.mark.parametrize("case_to_test", list(Case))
def test_model_update_dispatch_abstract(case_to_test):
    """Test that update dispatches correctly and case methods raise NotImplementedError."""
    model = Model()
    model.case = case_to_test
    with pytest.raises(NotImplementedError):
        model.update(data="test_data")

# Test Representation
def test_model_repr():
    """Test the __repr__ method."""
    model = Model(name="MyModel")
    assert repr(model) == "MyModel[NOM]"
    model.case = Case.INSTRUMENTAL
    assert repr(model) == "MyModel[INS]" 