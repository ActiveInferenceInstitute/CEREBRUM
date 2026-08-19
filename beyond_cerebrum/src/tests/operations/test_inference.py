"""Tests for FORMICA Bayesian inference operations (real API)."""

import pytest

from beyond_cerebrum.src.operations.inference import (
    ProbDistribution,
    update_belief,
    infer_meaning_bayesian,
    resolve_reference_bayesian,
    DummyLikelihoodModel,
    DummySyntaxSemanticModel,
    DummyReferenceLikelihoodModel,
    DummyDiscourseModel,
)


# --- Fixtures ---

@pytest.fixture
def prior_belief() -> ProbDistribution:
    return ProbDistribution({"state1": 0.6, "state2": 0.4})


@pytest.fixture
def dummy_observation() -> str:
    return "observed utterance"


class PreferState1Likelihood:
    """Real likelihood model: favors state1 by 3:1."""

    def get_likelihood(self, observation, state) -> float:
        return 0.9 if state == "state1" else 0.3


# --- update_belief ---

def test_update_belief_constant_likelihood_preserves_prior(prior_belief, dummy_observation):
    posterior = update_belief(prior_belief, dummy_observation, DummyLikelihoodModel())
    assert isinstance(posterior, ProbDistribution)
    assert posterior.get_prob("state1") == pytest.approx(0.6)
    assert posterior.get_prob("state2") == pytest.approx(0.4)


def test_update_belief_shifts_toward_likely_state(prior_belief, dummy_observation):
    posterior = update_belief(prior_belief, dummy_observation, PreferState1Likelihood())
    assert posterior.get_prob("state1") > prior_belief.get_prob("state1")
    assert posterior.get_prob("state1") + posterior.get_prob("state2") == pytest.approx(1.0)


def test_update_belief_rejects_model_without_get_likelihood(prior_belief, dummy_observation):
    with pytest.raises(TypeError):
        update_belief(prior_belief, dummy_observation, object())


def test_update_belief_zero_likelihood_concentrates_mass(prior_belief, dummy_observation):
    class ZeroState1:
        def get_likelihood(self, observation, state) -> float:
            return 0.0 if state == "state1" else 1.0

    posterior = update_belief(prior_belief, dummy_observation, ZeroState1())
    assert posterior.get_prob("state1") == pytest.approx(0.0)
    assert posterior.get_prob("state2") == pytest.approx(1.0)


def test_update_belief_negative_likelihood_clamped_to_zero(prior_belief, dummy_observation):
    class Negative:
        def get_likelihood(self, observation, state) -> float:
            return -1.0

    posterior = update_belief(prior_belief, dummy_observation, Negative())
    assert posterior.get_prob("state1") == 0.0
    assert posterior.get_prob("state2") == 0.0


def test_update_belief_erroring_model_zeroes_state(prior_belief, dummy_observation):
    class Exploding:
        def get_likelihood(self, observation, state):
            if state == "state1":
                raise ValueError("boom")
            return 1.0

    posterior = update_belief(prior_belief, dummy_observation, Exploding())
    assert posterior.get_prob("state1") == pytest.approx(0.0)
    assert posterior.get_prob("state2") == pytest.approx(1.0)


# --- infer_meaning_bayesian ---

def test_infer_meaning_bayesian_constant_likelihood_preserves_prior():
    prior = ProbDistribution({"meaning_a": 0.7, "meaning_b": 0.3})
    posterior = infer_meaning_bayesian(
        "syntax", {"topic": "test"}, prior, DummySyntaxSemanticModel()
    )
    assert posterior.get_prob("meaning_a") == pytest.approx(0.7)
    assert posterior.get_prob("meaning_b") == pytest.approx(0.3)


def test_infer_meaning_bayesian_rejects_bad_model():
    prior = ProbDistribution({"m": 1.0})
    with pytest.raises(TypeError):
        infer_meaning_bayesian("syntax", {}, prior, object())


# --- resolve_reference_bayesian ---

def test_resolve_reference_bayesian_constant_likelihood_preserves_prior():
    prior = ProbDistribution({"entity1": 0.8, "entity2": 0.2})
    posterior = resolve_reference_bayesian(
        "it", DummyDiscourseModel(), prior, DummyReferenceLikelihoodModel()
    )
    assert posterior.get_prob("entity1") == pytest.approx(0.8)
    assert posterior.get_prob("entity2") == pytest.approx(0.2)


def test_resolve_reference_bayesian_rejects_bad_discourse_model():
    prior = ProbDistribution({"e": 1.0})
    with pytest.raises(TypeError):
        resolve_reference_bayesian("it", object(), prior, DummyReferenceLikelihoodModel())


def test_resolve_reference_bayesian_rejects_bad_likelihood_model():
    prior = ProbDistribution({"e": 1.0})
    with pytest.raises(TypeError):
        resolve_reference_bayesian("it", DummyDiscourseModel(), prior, object())


# --- ProbDistribution basics ---

def test_prob_distribution_normalizes_on_construction():
    dist = ProbDistribution({"a": 2.0, "b": 2.0})
    assert dist.get_prob("a") == pytest.approx(0.5)
    assert dist.get_prob("b") == pytest.approx(0.5)


def test_prob_distribution_map_state():
    dist = ProbDistribution({"a": 0.2, "b": 0.8})
    assert dist.get_map_state() == "b"


def test_prob_distribution_map_state_empty():
    assert ProbDistribution().get_map_state() is None


def test_prob_distribution_map_state_all_zero():
    dist = ProbDistribution({"a": 0.0})
    assert dist.get_map_state() is None


def test_prob_distribution_sample_rejects_empty():
    with pytest.raises(ValueError):
        ProbDistribution().sample()
