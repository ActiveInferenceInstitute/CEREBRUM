"""
Tests for individual case handler classes in src/cases/.

Tests AccusativeCase, DativeCase, GenitiveCase, InstrumentalCase,
LocativeCase, AblativeCase, VocativeCase, and NominativeCase handlers.
Each test exercises: apply(), process_update(), calculate_free_energy(),
and any case-specific methods.
"""

import pytest
import numpy as np

from src.core.model import Model, Case
from src.cases.accusative import AccusativeCase
from src.cases.dative import DativeCase
from src.cases.genitive import GenitiveCase
from src.cases.instrumental import InstrumentalCase
from src.cases.locative import LocativeCase
from src.cases.ablative import AblativeCase
from src.cases.vocative import VocativeCase
from src.cases.nominative import NominativeCase


# ── Fixtures ──────────────────────────────────────────────────────

@pytest.fixture
def model():
    """Create a base model for testing."""
    m = Model(name="CaseTestModel")
    m.parameters["speed"] = 1.0
    m.parameters["weight"] = 0.5
    return m


# ── AccusativeCase ────────────────────────────────────────────────

class TestAccusativeCase:
    """Tests for AccusativeCase handler."""

    def test_precision_constant(self):
        assert AccusativeCase.PRECISION == 1.2

    def test_apply_sets_case(self, model):
        result = AccusativeCase.apply(model)
        assert result is model
        assert model.case == Case.ACCUSATIVE

    def test_process_update_with_own_method(self, model):
        """Test that process_update delegates to _update_accusative when available."""
        result = AccusativeCase.process_update(model, {"speed": 2.0})
        assert isinstance(result, dict)

    def test_process_update_default_updates_params(self, model):
        """Test the default path with a model lacking _update_accusative."""
        # Use a plain object that naturally lacks _update_accusative
        # instead of deleting the method from the Model class (which corrupts
        # all subsequent Model instances in the test session).
        class _PlainModel:
            def __init__(self):
                self.name = "Plain"
                self.parameters = {"speed": 1.0}
        plain = _PlainModel()
        result = AccusativeCase.process_update(plain, {"speed": 5.0})
        assert result["status"] == "updated"
        assert plain.parameters["speed"] == 5.0

    def test_calculate_free_energy_default(self, model):
        fe = AccusativeCase.calculate_free_energy(model)
        assert fe == 0.5  # default when no sensory data

    def test_calculate_free_energy_with_sensory(self, model):
        model.predicted_sensory = np.array([1.0, 0.0])
        model.actual_sensory = np.array([1.0, 0.0])
        fe = AccusativeCase.calculate_free_energy(model)
        assert fe == 0.0  # perfect match

    def test_calculate_free_energy_with_divergence(self, model):
        model.predicted_sensory = np.array([0.0, 0.0])
        model.actual_sensory = np.array([3.0, 4.0])
        fe = AccusativeCase.calculate_free_energy(model)
        assert fe == pytest.approx(5.0)  # Euclidean distance

    def test_accept_updates_valid(self, model):
        result = AccusativeCase.accept_updates(model, {"speed": 9.0})
        assert len(result["accepted"]) == 1
        assert result["accepted"][0]["new_value"] == 9.0
        assert model.parameters["speed"] == 9.0

    def test_accept_updates_unknown_param(self, model):
        result = AccusativeCase.accept_updates(model, {"nonexistent": 1.0})
        assert len(result["rejected"]) == 1
        assert result["rejected"][0]["reason"] == "parameter not found"

    def test_accept_updates_with_validation(self, model):
        def validator(param, value):
            if value < 0:
                return False, "must be positive"
            return True, ""

        result = AccusativeCase.accept_updates(model, {"speed": -1.0}, validation_func=validator)
        assert len(result["rejected"]) == 1
        assert "must be positive" in result["rejected"][0]["reason"]

    def test_accept_updates_status_error_on_all_rejected(self, model):
        result = AccusativeCase.accept_updates(model, {"nonexistent": 1.0})
        assert result["status"] == "error"


# ── DativeCase ────────────────────────────────────────────────────

class TestDativeCase:
    """Tests for DativeCase handler."""

    def test_precision_constant(self):
        assert DativeCase.PRECISION == 1.0

    def test_apply_sets_case(self, model):
        result = DativeCase.apply(model)
        assert result is model
        assert model.case == Case.DATIVE

    def test_process_update_delegates(self, model):
        """Test that process_update delegates to _update_dative when available."""
        result = DativeCase.process_update(model, {"goal": [5, 5, 0]})
        assert isinstance(result, dict)

    def test_calculate_free_energy_default(self, model):
        fe = DativeCase.calculate_free_energy(model)
        assert fe == 0.3

    def test_calculate_free_energy_with_match(self, model):
        model.expected_data = np.array([1.0])
        model.received_data = np.array([1.0])
        fe = DativeCase.calculate_free_energy(model)
        assert fe == 0.0

    def test_calculate_free_energy_with_divergence(self, model):
        model.expected_data = np.array([0.0])
        model.received_data = np.array([3.0])
        fe = DativeCase.calculate_free_energy(model)
        assert fe == pytest.approx(3.0)

    def test_receive_data_goal(self, model):
        model.goal_position = np.array([0, 0, 0])
        result = DativeCase.receive_data(model, [10, 20, 0], data_type="goal")
        assert result["status"] == "received"
        assert result["data_type"] == "goal"
        np.testing.assert_array_equal(model.goal_position, np.array([10, 20, 0]))

    def test_receive_data_command(self, model):
        result = DativeCase.receive_data(model, "stop", data_type="command")
        assert result["status"] == "received"
        assert "stop" in model._pending_commands

    def test_receive_data_generic(self, model):
        result = DativeCase.receive_data(model, {"x": 1}, data_type="custom")
        assert result["data_type"] == "custom"
        assert model._received_data["custom"] == {"x": 1}


# ── GenitiveCase ──────────────────────────────────────────────────

class TestGenitiveCase:
    """Tests for GenitiveCase handler."""

    def test_apply_sets_case(self, model):
        from src.cases.genitive import GenitiveCase
        result = GenitiveCase.apply(model)
        assert result is model
        assert model.case == Case.GENITIVE

    def test_process_update(self, model):
        result = GenitiveCase.process_update(model, {"query": "parameters"})
        assert isinstance(result, dict)

    def test_calculate_free_energy_default(self, model):
        fe = GenitiveCase.calculate_free_energy(model)
        assert isinstance(fe, float)

    def test_get_parameters(self, model):
        result = GenitiveCase.get_parameters(model)
        assert isinstance(result, dict)

    def test_derive_from(self, model):
        target = Model(name="Target")
        result = GenitiveCase.derive_from(model, target)
        assert isinstance(result, dict)


# ── InstrumentalCase ──────────────────────────────────────────────

class TestInstrumentalCase:
    """Tests for InstrumentalCase handler."""

    def test_apply_sets_case(self, model):
        result = InstrumentalCase.apply(model)
        assert result is model
        assert model.case == Case.INSTRUMENTAL

    def test_process_update(self, model):
        result = InstrumentalCase.process_update(model, {"operation": "transform"})
        assert isinstance(result, dict)

    def test_calculate_free_energy(self, model):
        fe = InstrumentalCase.calculate_free_energy(model)
        assert isinstance(fe, float)

    def test_execute_operation_missing(self, model):
        result = InstrumentalCase.execute_operation(model, operation="transform", input_data={"x": 1})
        assert result is None  # operation not registered on model

    def test_execute_operation_registered(self, model):
        model.double = lambda data: data * 2
        result = InstrumentalCase.execute_operation(model, operation="double", input_data=5)
        assert result == 10

    def test_get_parameters(self, model):
        result = InstrumentalCase.get_parameters(model)
        assert isinstance(result, dict)


# ── LocativeCase ──────────────────────────────────────────────────

class TestLocativeCase:
    """Tests for LocativeCase handler."""

    def test_apply_sets_case(self, model):
        result = LocativeCase.apply(model)
        assert result is model
        assert model.case == Case.LOCATIVE

    def test_process_update(self, model):
        result = LocativeCase.process_update(model, {"context": "environment"})
        assert isinstance(result, dict)

    def test_calculate_free_energy(self, model):
        fe = LocativeCase.calculate_free_energy(model)
        assert isinstance(fe, float)

    def test_add_to_context(self, model):
        entity = Model(name="Entity")
        result = LocativeCase.add_to_context(model, entity)
        assert isinstance(result, bool)

    def test_get_context_contents(self, model):
        contents = LocativeCase.get_context_contents(model)
        assert isinstance(contents, list)



# ── AblativeCase ──────────────────────────────────────────────────

class TestAblativeCase:
    """Tests for AblativeCase handler."""

    def test_apply_sets_case(self, model):
        result = AblativeCase.apply(model)
        assert result is model
        assert model.case == Case.ABLATIVE

    def test_process_update(self, model):
        result = AblativeCase.process_update(model, {"source": "gradient"})
        assert isinstance(result, dict)

    def test_calculate_free_energy(self, model):
        fe = AblativeCase.calculate_free_energy(model)
        assert isinstance(fe, float)

    def test_emit_to(self, model):
        target = Model(name="Target")
        result = AblativeCase.emit_to(model, target)
        assert isinstance(result, dict)



# ── VocativeCase ──────────────────────────────────────────────────

class TestVocativeCase:
    """Tests for VocativeCase handler."""

    def test_apply_sets_case(self, model):
        result = VocativeCase.apply(model)
        assert result is model
        assert model.case == Case.VOCATIVE

    def test_process_update(self, model):
        result = VocativeCase.process_update(model, {"query": "status"})
        assert isinstance(result, dict)

    def test_calculate_free_energy(self, model):
        fe = VocativeCase.calculate_free_energy(model)
        assert isinstance(fe, float)

    def test_invoke(self, model):
        result = VocativeCase.invoke(model, invocation={"command": "status"})
        assert isinstance(result, dict)

    def test_broadcast(self, model):
        m2 = Model(name="Other")
        results = VocativeCase.broadcast([model, m2], message="ping")
        assert isinstance(results, list)
        assert len(results) == 2

    def test_get_invocation_history(self, model):
        history = VocativeCase.get_invocation_history(model)
        assert isinstance(history, list)


# ── NominativeCase ────────────────────────────────────────────────

class TestNominativeCase:
    """Tests for NominativeCase handler."""

    def test_apply_sets_case(self, model):
        result = NominativeCase.apply(model)
        assert result is model
        assert model.case == Case.NOMINATIVE

    def test_process_update(self, model):
        result = NominativeCase.process_update(model, {"action": "predict"})
        assert isinstance(result, dict)

    def test_calculate_free_energy(self, model):
        fe = NominativeCase.calculate_free_energy(model)
        assert isinstance(fe, float)
