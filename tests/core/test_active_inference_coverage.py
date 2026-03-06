"""
Tests for src/core/active_inference.py

Comprehensive tests for ActiveInferenceModel covering initialization,
likelihood, free energy, posterior updates, predictions, and all 8
case-specific _update_* methods.
"""

import pytest
import numpy as np

from src.core.model import Case
from src.core.active_inference import ActiveInferenceModel


@pytest.fixture
def ai_model():
    """Simple 3-state ActiveInferenceModel."""
    return ActiveInferenceModel(
        name="TestAI",
        prior_means=np.array([0.3, 0.5, 0.2]),
        prior_precision=np.eye(3) * 2.0,
        likelihood_precision=np.eye(3) * 1.0,
    )


@pytest.fixture
def pomdp_model():
    """ActiveInferenceModel with full POMDP parameters."""
    n_states, n_actions, n_obs = 3, 2, 3
    params = {
        'n_states': n_states,
        'n_actions': n_actions,
        'n_observations': n_obs,
        'transition_matrix': np.random.dirichlet(np.ones(n_states), size=(n_states, n_actions)),
        'observation_matrix': np.random.dirichlet(np.ones(n_obs), size=n_states),
    }
    return ActiveInferenceModel(
        name="POMDP",
        parameters=params,
        prior_means=np.ones(n_states) / n_states,
    )


# ── Initialization ───────────────────────────────────────────────

class TestInit:
    def test_defaults(self):
        m = ActiveInferenceModel(name="Default")
        assert m.n_states == 1
        assert m.posterior_means.shape == (1,)

    def test_custom_prior(self, ai_model):
        assert ai_model.n_states == 3
        np.testing.assert_array_equal(ai_model.posterior_means, ai_model.prior_means)

    def test_1d_precision_converted_to_diag(self):
        m = ActiveInferenceModel(
            name="1D",
            prior_means=np.array([0.5, 0.5]),
            prior_precision=np.array([2.0, 3.0]),
            likelihood_precision=np.array([1.0, 1.0]),
        )
        assert m.prior_precision.shape == (2, 2)
        assert m.prior_precision[0, 0] == 2.0
        assert m.prior_precision[1, 1] == 3.0

    def test_pomdp_init(self, pomdp_model):
        assert pomdp_model.parameters['n_states'] == 3
        assert pomdp_model.case == Case.NOMINATIVE


# ── Likelihood & Prediction Error ─────────────────────────────────

class TestLikelihoodAndError:
    def test_likelihood_identity(self, ai_model):
        states = np.array([1.0, 2.0, 3.0])
        np.testing.assert_array_equal(ai_model.likelihood(states), states)

    def test_prediction_error(self, ai_model):
        obs = np.array([1.0, 1.0, 1.0])
        error = ai_model.prediction_error(obs)
        expected = obs - ai_model.posterior_means
        np.testing.assert_array_almost_equal(error, expected)


# ── Free Energy ───────────────────────────────────────────────────

class TestFreeEnergy:
    def test_free_energy_no_obs(self, ai_model):
        fe = ai_model.free_energy()
        assert isinstance(fe, (float, np.floating))

    def test_free_energy_with_obs(self, ai_model):
        obs = np.array([0.4, 0.5, 0.1])
        fe = ai_model.free_energy(obs)
        assert isinstance(fe, (float, np.floating))
        assert len(ai_model.free_energy_history) == 1

    def test_free_energy_zero_at_prior(self):
        m = ActiveInferenceModel(name="Zero", prior_means=np.zeros(2))
        fe = m.free_energy()
        assert fe == pytest.approx(0.0, abs=1e-10)


# ── Posterior Update ──────────────────────────────────────────────

class TestPosteriorUpdate:
    def test_simplified_update(self, ai_model):
        obs = np.array([1.0, 0.0, 0.0])
        result = ai_model.update_posterior(obs)
        assert result['status'] == 'success'
        assert result['method'] == 'simplified'

    def test_dimension_mismatch(self, ai_model):
        obs = np.array([1.0, 0.0])  # wrong dim
        result = ai_model.update_posterior(obs)
        assert result['status'] == 'error'

    def test_bayesian_update(self, pomdp_model):
        obs = np.array([1.0, 0.0, 0.0])  # one-hot
        result = pomdp_model.update_posterior(obs)
        assert result['status'] == 'success'
        assert result['method'] == 'bayesian'


# ── Optimal Action & Predictions ──────────────────────────────────

class TestPredictions:
    def test_get_optimal_action(self, pomdp_model):
        action = pomdp_model.get_optimal_action()
        assert 0 <= action < pomdp_model.parameters['n_actions']

    def test_predict_next_state(self, pomdp_model):
        dist = pomdp_model.predict_next_state(0)
        assert dist.shape == (3,)
        assert np.sum(dist) == pytest.approx(1.0, abs=0.1)

    def test_predict_observation(self, pomdp_model):
        obs = pomdp_model.predict_observation()
        assert obs.shape == (3,)

    def test_predict_observation_custom_dist(self, pomdp_model):
        custom = np.array([1.0, 0.0, 0.0])
        obs = pomdp_model.predict_observation(custom)
        assert obs.shape == (3,)


# ── Case-Specific Updates ────────────────────────────────────────

class TestCaseUpdates:
    def test_nominative_no_data(self, ai_model):
        result = ai_model._update_nominative(None)
        assert result['status'] == 'success'
        assert 'predictions' in result

    def test_nominative_with_data(self, ai_model):
        result = ai_model._update_nominative([0.5, 0.5, 0.0])
        assert result['status'] == 'success'
        assert 'prediction_error' in result

    def test_accusative_no_data(self, ai_model):
        result = ai_model._update_accusative(None)
        assert result['status'] == 'error'

    def test_accusative_with_data(self, ai_model):
        result = ai_model._update_accusative([0.5, 0.3, 0.2])
        assert result['status'] == 'success'

    def test_genitive_no_data(self, ai_model):
        result = ai_model._update_genitive(None)
        assert result['status'] == 'success'
        assert 'products' in result

    def test_genitive_with_samples(self, ai_model):
        result = ai_model._update_genitive({"n_samples": 3})
        assert result['status'] == 'success'
        assert len(result['products']) == 3

    def test_dative_no_data(self, ai_model):
        result = ai_model._update_dative(None)
        assert result['status'] == 'error'

    def test_dative_with_data(self, ai_model):
        result = ai_model._update_dative([0.5, 0.3, 0.2])
        assert result['status'] == 'success'
        assert 'prediction_error' in result

    def test_instrumental_no_data(self, ai_model):
        result = ai_model._update_instrumental(None)
        assert result['status'] == 'error'

    def test_instrumental_predict(self, ai_model):
        result = ai_model._update_instrumental({"method": "predict"})
        assert result['status'] == 'success'
        assert result['method'] == 'predict'

    def test_instrumental_invert(self, ai_model):
        result = ai_model._update_instrumental({
            "method": "invert",
            "params": {"observations": [0.5, 0.3, 0.2], "iterations": 3}
        })
        assert result['status'] == 'success'
        assert result['method'] == 'invert'

    def test_instrumental_unknown_method(self, ai_model):
        result = ai_model._update_instrumental({"method": "dance"})
        assert result['status'] == 'error'

    def test_locative_no_data(self, ai_model):
        result = ai_model._update_locative(None)
        assert result['status'] == 'success'
        assert 'prior_means' in result['context']

    def test_locative_list_params(self, ai_model):
        result = ai_model._update_locative(["n_states", "posterior_means"])
        assert result['status'] == 'success'
        assert 'n_states' in result['context']

    def test_locative_dict_params(self, ai_model):
        result = ai_model._update_locative({"n_states": None})
        assert result['status'] == 'success'
        assert result['context']['n_states'] == 3

    def test_ablative_no_data(self, ai_model):
        result = ai_model._update_ablative(None)
        assert result['status'] == 'success'
        assert 'history' in result

    def test_ablative_beliefs(self, ai_model):
        result = ai_model._update_ablative("beliefs")
        assert result['status'] == 'success'

    def test_ablative_free_energy(self, ai_model):
        result = ai_model._update_ablative("free_energy")
        assert result['status'] == 'success'

    def test_ablative_transitions(self, ai_model):
        result = ai_model._update_ablative("transitions")
        assert result['status'] == 'success'

    def test_ablative_invalid(self, ai_model):
        result = ai_model._update_ablative(42)
        assert result['status'] == 'error'

    def test_vocative_no_data(self, ai_model):
        result = ai_model._update_vocative(None)
        assert result['status'] == 'success'
        assert 'TestAI' in result['response']

    def test_vocative_identify(self, ai_model):
        result = ai_model._update_vocative("identify")
        assert result['status'] == 'success'
        assert result['identity']['name'] == 'TestAI'

    def test_vocative_status(self, ai_model):
        result = ai_model._update_vocative("status")
        assert result['status'] == 'success'

    def test_vocative_help(self, ai_model):
        result = ai_model._update_vocative("help")
        assert result['status'] == 'success'
        assert 'identify' in result['commands']

    def test_vocative_capabilities(self, ai_model):
        result = ai_model._update_vocative("capabilities")
        assert result['status'] == 'success'
        assert result['capabilities']['states'] == 3

    def test_vocative_unknown(self, ai_model):
        result = ai_model._update_vocative("explode")
        assert result['status'] == 'error'
