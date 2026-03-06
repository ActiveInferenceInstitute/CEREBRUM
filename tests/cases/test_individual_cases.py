"""
Tests for individual case implementations in src/cases/.

Tests each case handler (NominativeCase, AccusativeCase, etc.) to verify:
- apply() sets the correct case on a model
- process_update() returns valid result dictionaries
- calculate_free_energy() returns numeric values
- get_parameters() returns parameter dictionaries
"""

import pytest
import numpy as np

from src.core.model import Model, Case
from src.cases.nominative import NominativeCase
from src.cases.accusative import AccusativeCase
from src.cases.dative import DativeCase
from src.cases.genitive import GenitiveCase
from src.cases.instrumental import InstrumentalCase
from src.cases.locative import LocativeCase
from src.cases.ablative import AblativeCase
from src.cases.vocative import VocativeCase


@pytest.fixture
def base_model():
    """Create a base model for testing case applications."""
    model = Model(name="TestCaseModel")
    model.parameters = {
        "action_threshold": 0.5,
        "prediction_horizon": 3,
        "action_cost": 0.1,
        "exploration_rate": 0.2,
        "learning_rate": 0.01,
        "precision": 1.0,
    }
    return model


# ─── Parametrized tests across all case handlers ───

CASE_HANDLERS = [
    (NominativeCase, Case.NOMINATIVE),
    (AccusativeCase, Case.ACCUSATIVE),
    (DativeCase, Case.DATIVE),
    (GenitiveCase, Case.GENITIVE),
    (InstrumentalCase, Case.INSTRUMENTAL),
    (LocativeCase, Case.LOCATIVE),
    (AblativeCase, Case.ABLATIVE),
    (VocativeCase, Case.VOCATIVE),
]


@pytest.mark.parametrize("handler_cls,expected_case", CASE_HANDLERS)
class TestCaseApply:
    """Test that apply() correctly configures the model's case."""

    def test_apply_returns_model(self, handler_cls, expected_case, base_model):
        result = handler_cls.apply(base_model)
        assert result is base_model

    def test_apply_sets_correct_case(self, handler_cls, expected_case, base_model):
        handler_cls.apply(base_model)
        assert base_model.case == expected_case


@pytest.mark.parametrize("handler_cls,expected_case", CASE_HANDLERS)
class TestCaseProcessUpdate:
    """Test that process_update() returns valid result dictionaries."""

    def test_returns_dict(self, handler_cls, expected_case, base_model):
        handler_cls.apply(base_model)
        result = handler_cls.process_update(base_model, np.array([1.0, 2.0]))
        assert isinstance(result, dict)

    def test_result_has_status(self, handler_cls, expected_case, base_model):
        handler_cls.apply(base_model)
        result = handler_cls.process_update(base_model, np.array([1.0, 2.0]))
        # All case updates should report some status
        assert len(result) >= 1


@pytest.mark.parametrize("handler_cls,expected_case", CASE_HANDLERS)
class TestCaseCalculateFreeEnergy:
    """Test that calculate_free_energy() returns numeric values."""

    def test_returns_numeric(self, handler_cls, expected_case, base_model):
        handler_cls.apply(base_model)
        fe = handler_cls.calculate_free_energy(base_model)
        assert isinstance(fe, (int, float, np.floating))

    def test_returns_finite(self, handler_cls, expected_case, base_model):
        handler_cls.apply(base_model)
        fe = handler_cls.calculate_free_energy(base_model)
        assert np.isfinite(fe)


# Cases that implement get_parameters()
CASE_HANDLERS_WITH_PARAMS = [
    (h, c) for h, c in CASE_HANDLERS
    if hasattr(h, 'get_parameters')
]


@pytest.mark.parametrize("handler_cls,expected_case", CASE_HANDLERS_WITH_PARAMS)
class TestCaseGetParameters:
    """Test that get_parameters() returns parameter dictionaries."""

    def test_returns_dict(self, handler_cls, expected_case, base_model):
        handler_cls.apply(base_model)
        params = handler_cls.get_parameters(base_model)
        assert isinstance(params, dict)

    def test_params_non_empty(self, handler_cls, expected_case, base_model):
        handler_cls.apply(base_model)
        params = handler_cls.get_parameters(base_model)
        assert len(params) >= 1

    def test_params_values_have_valid_types(self, handler_cls, expected_case, base_model):
        handler_cls.apply(base_model)
        params = handler_cls.get_parameters(base_model)
        for key, value in params.items():
            assert isinstance(value, (int, float, np.floating, np.integer, str, bool)), \
                f"Parameter {key} is {type(value)}, expected numeric or string"


# ─── Specific case behavior tests ───


class TestNominativeCaseSpecifics:
    """Test nominative-case specific behaviors."""

    def test_free_energy_with_goal_state(self):
        model = Model(name="GoalModel")
        model.goal_state = np.array([1.0, 0.0])
        model.current_state = np.array([0.5, 0.5])
        NominativeCase.apply(model)
        fe = NominativeCase.calculate_free_energy(model)
        expected = np.linalg.norm(model.current_state - model.goal_state)
        assert abs(fe - expected) < 1e-10

    def test_free_energy_without_goal_state(self):
        model = Model(name="NoGoalModel")
        NominativeCase.apply(model)
        fe = NominativeCase.calculate_free_energy(model)
        # Should return default 1.0 when no goal_state
        assert fe == 1.0


class TestCaseSequence:
    """Test applying cases in sequence."""

    def test_case_transition_sequence(self, base_model):
        for handler_cls, expected_case in CASE_HANDLERS:
            handler_cls.apply(base_model)
            assert base_model.case == expected_case
        # After applying all cases, model should be in last case
        assert base_model.case == Case.VOCATIVE

    def test_all_cases_produce_parameters(self, base_model):
        for handler_cls, _ in CASE_HANDLERS_WITH_PARAMS:
            handler_cls.apply(base_model)
            params = handler_cls.get_parameters(base_model)
            assert isinstance(params, dict) and len(params) >= 1
