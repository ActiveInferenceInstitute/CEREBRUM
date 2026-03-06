"""
Targeted tests for uncovered branches in case handler modules.

Boosts coverage for dative (60→90%), genitive (63→90%), instrumental (66→90%),
locative (67→90%), ablative (69→90%), nominative (77→90%), vocative (75→90%).

Tests the DEFAULT code paths (when model lacks internal _update_* methods),
calculate_free_energy with model attributes, and case-specific utility methods.
"""

import pytest
import numpy as np

from src.core.model import Model, Case
from src.cases.dative import DativeCase
from src.cases.genitive import GenitiveCase
from src.cases.instrumental import InstrumentalCase
from src.cases.locative import LocativeCase
from src.cases.ablative import AblativeCase
from src.cases.nominative import NominativeCase
from src.cases.vocative import VocativeCase


@pytest.fixture
def bare_model():
    """Model instance for tests that need real Model methods."""
    return Model(name="BareModel")


class _BareModel:
    """Minimal stand-in that lacks _update_* methods."""
    def __init__(self):
        self.name = "BareModel"
        self.id = "bare-id"
        self.case = Case.NOMINATIVE
        self.parameters = {}
        self._case_history = []
        self._prior_case = None
        self._case = Case.NOMINATIVE


@pytest.fixture
def truly_bare():
    """Object that has NO _update_* methods at all."""
    return _BareModel()


# ── Dative Default Paths ─────────────────────────────────────────

class TestDativeDefaultPaths:
    def test_process_update_default_goal(self, truly_bare):
        bare_model = truly_bare
        bare_model.goal_position = np.zeros(2)
        result = DativeCase.process_update(bare_model, {"goal": [5.0, 5.0]})
        assert "goal" in result["data_processed"]
        np.testing.assert_array_equal(bare_model.goal_position, [5.0, 5.0])

    def test_process_update_goal_position_key(self, truly_bare):
        bare_model = truly_bare
        bare_model.goal_position = np.zeros(2)
        result = DativeCase.process_update(bare_model, {"goal_position": [3.0, 4.0]})
        assert "goal" in result["data_processed"]

    def test_process_update_goal_attr_fallback(self, truly_bare):
        bare_model = truly_bare
        bare_model.goal = None
        result = DativeCase.process_update(bare_model, {"goal": [1, 2]})
        assert "goal" in result["data_processed"]

    def test_process_update_sensory_data(self, truly_bare):
        bare_model = truly_bare
        bare_model.sensory = None
        result = DativeCase.process_update(bare_model, {"sensory_data": [0.1, 0.2]})
        assert "sensory_data" in result["data_processed"]

    def test_process_update_message(self, truly_bare):
        bare_model = truly_bare
        bare_model.messages = []
        result = DativeCase.process_update(bare_model, {"message": "hello"})
        assert "message" in result["data_processed"]
        assert "hello" in bare_model.messages

    def test_process_update_message_non_list(self, truly_bare):
        bare_model = truly_bare
        bare_model.messages = "not_a_list"
        result = DativeCase.process_update(bare_model, {"message": "hello"})
        assert "message" in result["data_processed"]
        assert bare_model.messages == ["hello"]

    def test_calc_free_energy_with_arrays(self, bare_model):
        bare_model.expected_data = np.array([1.0, 0.0])
        bare_model.received_data = np.array([0.5, 0.5])
        fe = DativeCase.calculate_free_energy(bare_model)
        assert fe > 0

    def test_calc_free_energy_match(self, bare_model):
        bare_model.expected_data = "abc"
        bare_model.received_data = "abc"
        fe = DativeCase.calculate_free_energy(bare_model)
        assert fe == 0.0

    def test_calc_free_energy_no_match(self, bare_model):
        bare_model.expected_data = "abc"
        bare_model.received_data = "xyz"
        fe = DativeCase.calculate_free_energy(bare_model)
        assert fe == 0.3

    def test_receive_data_goal(self, bare_model):
        bare_model.goal_position = np.zeros(2)
        result = DativeCase.receive_data(bare_model, [5.0, 3.0], data_type="goal")
        assert result["data_type"] == "goal"

    def test_receive_data_perception(self, bare_model):
        bare_model.vision_field = np.zeros(5)
        result = DativeCase.receive_data(bare_model, [1, 1, 1, 1, 1], data_type="perception")
        assert result["data_type"] == "perception"

    def test_receive_data_command(self, bare_model):
        result = DativeCase.receive_data(bare_model, "turn_left", data_type="command")
        assert result["data_type"] == "command"
        assert bare_model._pending_commands == ["turn_left"]

    def test_receive_data_generic(self, bare_model):
        result = DativeCase.receive_data(bare_model, {"x": 1}, data_type="custom")
        assert result["data_type"] == "custom"


# ── Genitive Default Paths ────────────────────────────────────────

class TestGenitiveDefaultPaths:
    def test_process_update_default_path(self, truly_bare):
        result = GenitiveCase.process_update(truly_bare, None)
        assert result["status"] == "source"
        assert result["role"] == "providing"

    def test_calc_free_energy_with_params(self, bare_model):
        bare_model.parameters = {"speed": 1.0, "range": 5.0}
        bare_model.expected_parameters = {"speed": 1.5, "range": 5.0}
        fe = GenitiveCase.calculate_free_energy(bare_model)
        assert fe == pytest.approx(0.25 * GenitiveCase.PRECISION)

    def test_calc_free_energy_default(self, bare_model):
        fe = GenitiveCase.calculate_free_energy(bare_model)
        assert fe == 1.0

    def test_derive_from_non_dict(self, bare_model):
        target = Model(name="Target")
        bare_model.state = 42  # non-dict attribute
        derived = GenitiveCase.derive_from(bare_model, target, attributes=["state"])
        assert derived["state"] == 42


# ── Instrumental Default Paths ────────────────────────────────────

class TestInstrumentalDefaultPaths:
    def test_process_update_default_path(self, truly_bare):
        result = InstrumentalCase.process_update(truly_bare, None)
        assert result["status"] == "tool"

    def test_calc_free_energy_with_costs(self, bare_model):
        bare_model.operation_cost = 2.0
        bare_model.expected_cost = 1.5
        fe = InstrumentalCase.calculate_free_energy(bare_model)
        assert fe == pytest.approx(0.5 * InstrumentalCase.PRECISION)

    def test_calc_free_energy_default(self, bare_model):
        fe = InstrumentalCase.calculate_free_energy(bare_model)
        assert fe == 1.0

    def test_register_operation(self, bare_model):
        InstrumentalCase.apply(bare_model)
        fn = lambda x: x * 2
        InstrumentalCase.register_operation(bare_model, "double", fn)
        assert "double" in bare_model._instrument_operations
        assert bare_model.double(5) == 10

    def test_register_operation_without_prior_apply(self, bare_model):
        fn = lambda x: x + 1
        InstrumentalCase.register_operation(bare_model, "inc", fn)
        assert hasattr(bare_model, '_instrument_operations')


# ── Locative Default Paths ────────────────────────────────────────

class TestLocativeDefaultPaths:
    def test_process_update_default(self, truly_bare):
        LocativeCase.apply(truly_bare)
        result = LocativeCase.process_update(truly_bare, None)
        assert result["status"] == "context"
        assert result["contents_count"] == 0

    def test_calc_free_energy_with_stability(self, bare_model):
        bare_model.context_stability = 0.8
        fe = LocativeCase.calculate_free_energy(bare_model)
        assert fe == pytest.approx(0.2 * LocativeCase.PRECISION)

    def test_calc_free_energy_default(self, bare_model):
        fe = LocativeCase.calculate_free_energy(bare_model)
        assert fe == 1.0

    def test_add_to_context(self, bare_model):
        LocativeCase.apply(bare_model)
        result = LocativeCase.add_to_context(bare_model, "entity_1", {"type": "agent"})
        assert result is True

    def test_add_to_context_at_capacity(self, bare_model):
        LocativeCase.apply(bare_model)
        bare_model.parameters["max_capacity"] = 1
        LocativeCase.add_to_context(bare_model, "e1")
        assert LocativeCase.add_to_context(bare_model, "e2") is False

    def test_remove_from_context(self, bare_model):
        LocativeCase.apply(bare_model)
        LocativeCase.add_to_context(bare_model, "entity_x")
        assert LocativeCase.remove_from_context(bare_model, "entity_x") is True
        assert LocativeCase.remove_from_context(bare_model, "entity_x") is False

    def test_remove_from_context_no_attr(self, bare_model):
        assert LocativeCase.remove_from_context(bare_model, "x") is False

    def test_get_context_contents(self, bare_model):
        LocativeCase.apply(bare_model)
        LocativeCase.add_to_context(bare_model, "e1")
        LocativeCase.add_to_context(bare_model, "e2")
        contents = LocativeCase.get_context_contents(bare_model)
        assert contents == ["e1", "e2"]

    def test_get_context_contents_empty(self, bare_model):
        assert LocativeCase.get_context_contents(bare_model) == []



# ── Ablative Default Paths ────────────────────────────────────────

class TestAblativeDefaultPaths:
    def test_process_update_default(self, truly_bare):
        result = AblativeCase.process_update(truly_bare, None)
        assert result["status"] == "origin"

    def test_calc_free_energy_with_emissions(self, bare_model):
        AblativeCase.apply(bare_model)
        bare_model._ablative_emissions = [1.0, 2.0, 3.0, 1.0, 2.0]
        fe = AblativeCase.calculate_free_energy(bare_model)
        assert fe > 0

    def test_calc_free_energy_default(self, bare_model):
        fe = AblativeCase.calculate_free_energy(bare_model)
        assert fe == 1.0

    def test_emit_to_no_payload(self, bare_model):
        target = Model(name="Target")
        AblativeCase.apply(bare_model)
        result = AblativeCase.emit_to(bare_model, target)
        assert result["from"] == "BareModel"
        assert result["to"] == "Target"

    def test_emit_to_with_payload(self, bare_model):
        target = Model(name="T2")
        AblativeCase.apply(bare_model)
        result = AblativeCase.emit_to(bare_model, target, payload={"data": 42})
        assert result["payload"]["data"] == 42

    def test_emit_to_current_state(self, bare_model):
        target = Model(name="T3")
        AblativeCase.apply(bare_model)
        bare_model.current_state = "active"
        result = AblativeCase.emit_to(bare_model, target)
        assert result["payload"] == "active"



# ── Nominative Default Paths ─────────────────────────────────────

class TestNominativeDefaultPaths:
    def test_process_update_default(self, truly_bare):
        result = NominativeCase.process_update(truly_bare, None)
        assert result["status"] == "active"
        assert result["action"] == "generated"

    def test_process_update_with_generate_action(self, truly_bare):
        truly_bare.generate_action = lambda data: {"move": "forward"}
        result = NominativeCase.process_update(truly_bare, {"env": {}})
        assert result["action_details"] == {"move": "forward"}

    def test_calc_free_energy_default(self, bare_model):
        fe = NominativeCase.calculate_free_energy(bare_model)
        assert fe == 1.0

    def test_calc_free_energy_array_states(self, bare_model):
        bare_model.goal_state = np.array([1.0, 0.0])
        bare_model.current_state = np.array([0.0, 0.0])
        fe = NominativeCase.calculate_free_energy(bare_model)
        assert fe == pytest.approx(1.0)

    def test_calc_free_energy_scalar_match(self, bare_model):
        bare_model.goal_state = "done"
        bare_model.current_state = "done"
        fe = NominativeCase.calculate_free_energy(bare_model)
        assert fe == 0.0

    def test_calc_free_energy_scalar_no_match(self, bare_model):
        bare_model.goal_state = "done"
        bare_model.current_state = "running"
        fe = NominativeCase.calculate_free_energy(bare_model)
        assert fe == 1.0

    def test_get_parameters(self, bare_model):
        params = NominativeCase.get_parameters(bare_model)
        assert "action_threshold" in params
        assert "prediction_horizon" in params


# ── Vocative Default Paths ────────────────────────────────────────

class TestVocativeDefaultPaths:
    def test_process_update_default(self, truly_bare):
        VocativeCase.apply(truly_bare)
        result = VocativeCase.process_update(truly_bare, {"msg": "hi"})
        assert result["status"] == "addressed"
        assert result["attention"] == "high"
        assert len(truly_bare._vocative_invocations) == 1

    def test_process_update_with_respond(self, truly_bare):
        VocativeCase.apply(truly_bare)
        truly_bare.respond = lambda data: "pong"
        result = VocativeCase.process_update(truly_bare, "ping")
        assert result["response"] == "pong"

    def test_calc_free_energy_default(self, bare_model):
        fe = VocativeCase.calculate_free_energy(bare_model)
        assert fe == 1.0

    def test_calc_free_energy_with_latency(self, bare_model):
        bare_model.response_latency = 0.5
        fe = VocativeCase.calculate_free_energy(bare_model)
        assert fe == pytest.approx(0.5 * VocativeCase.PRECISION)

    def test_invoke_default(self, bare_model):
        VocativeCase.apply(bare_model)
        result = VocativeCase.invoke(bare_model, {"command": "status"})
        assert result["acknowledged"] is True
        assert result["content"] == {"status": "acknowledged"}

    def test_invoke_with_respond(self, bare_model):
        VocativeCase.apply(bare_model)
        bare_model.respond = lambda inv: "ok"
        result = VocativeCase.invoke(bare_model, {"cmd": "go"})
        assert result["content"] == "ok"

    def test_invoke_with_process_fallback(self, bare_model):
        VocativeCase.apply(bare_model)
        bare_model.process = lambda inv: "processed"
        result = VocativeCase.invoke(bare_model, {"cmd": "x"})
        assert result["content"] == "processed"

    def test_get_parameters(self, bare_model):
        params = VocativeCase.get_parameters(bare_model)
        assert "response_timeout" in params
        assert "priority_level" in params

    def test_get_invocation_history_none(self, bare_model):
        history = VocativeCase.get_invocation_history(bare_model)
        assert history == []

    def test_get_invocation_history_with_data(self, bare_model):
        VocativeCase.apply(bare_model)
        VocativeCase.invoke(bare_model, {"a": 1})
        VocativeCase.invoke(bare_model, {"b": 2})
        history = VocativeCase.get_invocation_history(bare_model)
        assert len(history) == 2
