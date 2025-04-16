"""Tests for FORMICA inference operations."""

import pytest
import numpy as np
from typing import Any, Dict, Optional

from beyond_cerebrum.src.formalisms.structures import (
    AbstractStructure, SemanticGraph, GraphNode, SemanticGraphNode
)
from beyond_cerebrum.src.formalisms.types import PragmaticContext, SemanticConcept
from beyond_cerebrum.src.operations.inference import (
    ProbDistribution,
    update_belief,
    infer_meaning,
    resolve_reference_bayesian,
    # Import dummy models for testing
    DummyLikelihoodModel,
    DummySyntaxSemanticModel,
    DummyReferenceLikelihoodModel,
    DummyDiscourseModel
)

# --- Fixtures ---

# Define a simple state type for testing update_belief
StateType = str

@pytest.fixture
def prior_belief() -> ProbDistribution[StateType]:
    return ProbDistribution[StateType]({"state1": 0.6, "state2": 0.4})

@pytest.fixture
def dummy_observation() -> AbstractStructure:
    # A simple structure to act as an observation
    class SimpleObs(AbstractStructure):
        def validate(self) -> bool: return True
        def get_complexity(self) -> float: return 1.0
        def __repr__(self): return "SimpleObsInstance"
    return SimpleObs()

@pytest.fixture
def prior_meaning() -> ProbDistribution[SemanticGraph]:
    # Create two distinct dummy semantic graphs
    graph1 = SemanticGraph()
    graph1.add_node(SemanticGraphNode(id="g1c1", content=SemanticConcept("conceptA")))
    graph2 = SemanticGraph()
    graph2.add_node(SemanticGraphNode(id="g2c1", content=SemanticConcept("conceptB")))
    # Make them hashable for the dictionary (use a simple proxy)
    # NOTE: Real SemanticGraphs might need proper __hash__ based on content
    graph_map = { "graph1_proxy": graph1, "graph2_proxy": graph2 }
    dist = ProbDistribution[Any]({ "graph1_proxy": 0.7, "graph2_proxy": 0.3 })
    # Hack: Replace keys with actual graphs after creation (ProbDist doesn't use keys after init)
    dist._dist = { graph_map[k]: v for k, v in dist._dist.items() }
    return dist

@pytest.fixture
def prior_entities() -> ProbDistribution[Any]:
    # Simple string entities for testing reference resolution
    return ProbDistribution[str]({"entity1": 0.5, "entity2": 0.5})

@pytest.fixture
def dummy_context() -> PragmaticContext:
    return PragmaticContext({'speaker': 'tester'})

@pytest.fixture
def dummy_syntax() -> AbstractStructure:
    # Another simple structure for syntax
    class SimpleSyn(AbstractStructure):
        def validate(self) -> bool: return True
        def get_complexity(self) -> float: return 1.0
        def __repr__(self): return "SimpleSynInstance"
    return SimpleSyn()

# --- ProbDistribution Tests ---

def test_prob_distribution_init():
    dist = ProbDistribution[str]({"a": 0.6, "b": 0.4})
    assert dist.get_prob("a") == pytest.approx(0.6)
    assert dist.get_prob("b") == pytest.approx(0.4)
    assert dist.get_prob("c") == 0.0

def test_prob_distribution_normalization():
    dist = ProbDistribution[str]({"a": 3, "b": 2}) # Unnormalized
    assert dist.get_prob("a") == pytest.approx(0.6)
    assert dist.get_prob("b") == pytest.approx(0.4)

def test_prob_distribution_map_state():
    dist = ProbDistribution[str]({"a": 0.1, "b": 0.8, "c": 0.1})
    assert dist.get_map_state() == "b"

def test_prob_distribution_empty():
    dist = ProbDistribution[str]()
    assert dist.get_map_state() is None
    with pytest.raises(ValueError):
        dist.sample()

# --- Inference Operation Tests ---

def test_update_belief(prior_belief, dummy_observation):
    """Test the Bayesian update operation with a dummy likelihood."""
    likelihood_model = DummyLikelihoodModel() # Always returns 0.5
    posterior = update_belief(prior_belief, dummy_observation, likelihood_model)
    
    # Since likelihood is constant, posterior should equal prior
    assert isinstance(posterior, ProbDistribution)
    assert posterior.get_prob("state1") == pytest.approx(0.6)
    assert posterior.get_prob("state2") == pytest.approx(0.4)

def test_infer_meaning(dummy_syntax, dummy_context, prior_meaning):
    """Test meaning inference with a dummy model."""
    syntax_semantic_model = DummySyntaxSemanticModel() # Always returns 0.5
    posterior = infer_meaning(dummy_syntax, dummy_context, prior_meaning, syntax_semantic_model)
    
    assert isinstance(posterior, ProbDistribution)
    # Posterior should equal prior due to constant likelihood
    # Need to compare probabilities carefully as graphs might not be easily comparable keys
    graph1_prob = 0.0
    graph2_prob = 0.0
    for graph, prob in posterior._dist.items():
        if graph.nodes["g1c1"].content == SemanticConcept("conceptA"): 
            graph1_prob = prob
        elif graph.nodes["g2c1"].content == SemanticConcept("conceptB"): 
            graph2_prob = prob
            
    assert graph1_prob == pytest.approx(0.7)
    assert graph2_prob == pytest.approx(0.3)

def test_resolve_reference_bayesian(prior_entities):
    """Test reference resolution with dummy models."""
    discourse_model = DummyDiscourseModel()
    likelihood_model = DummyReferenceLikelihoodModel() # Always returns 0.5
    expression = "it"
    
    posterior = resolve_reference_bayesian(
        expression, discourse_model, prior_entities, likelihood_model
    )
    
    assert isinstance(posterior, ProbDistribution)
    # Posterior equals prior
    assert posterior.get_prob("entity1") == pytest.approx(0.5)
    assert posterior.get_prob("entity2") == pytest.approx(0.5)

def test_update_belief_missing_method():
    """Test that update_belief raises TypeError if model lacks get_likelihood."""
    prior = ProbDistribution[str]({"a": 1.0})
    obs = SimpleObs()
    bad_model = object() # Doesn't have the method
    with pytest.raises(TypeError, match="Likelihood model must have a 'get_likelihood' method"): 
        update_belief(prior, obs, bad_model)

# Define SimpleObs locally for the test above
class SimpleObs(AbstractStructure):
    def validate(self) -> bool: return True
    def get_complexity(self) -> float: return 1.0
    def __repr__(self): return "SimpleObsInstance" 