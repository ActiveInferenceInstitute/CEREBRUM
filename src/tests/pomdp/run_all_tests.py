#!/usr/bin/env python3
"""
POMDP Test Runner - Run all case tests and generate visualizations/animations.

This script runs all the grammatical case tests for POMDP models, generating
visualizations and animations for each case.
"""

import os
import logging
from typing import Dict, Any
import numpy as np

from src.core.model import Case
from src.tests.pomdp.test_cases import (
    test_nominative_case,
    test_locative_case,
    test_ablative_case,
    test_vocative_case
)

# Configure logging
logging.basicConfig(level=logging.INFO, 
                    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger("POMDP-Tests")

def generate_test_data() -> Dict[str, Any]:
    """
    Generate test data for POMDP models.
    
    Returns:
        Dictionary containing test data for POMDP models
    """
    # Create a small POMDP for testing
    n_states = 3
    n_actions = 2
    n_observations = 4
    
    # Generate transition matrix (n_states x n_actions x n_states)
    transition_matrix = np.zeros((n_states, n_actions, n_states))
    for s in range(n_states):
        for a in range(n_actions):
            probs = np.random.dirichlet(alpha=np.ones(n_states) * 0.5)
            transition_matrix[s, a, :] = probs
    
    # Generate observation matrix (n_states x n_observations)
    observation_matrix = np.zeros((n_states, n_observations))
    for s in range(n_states):
        probs = np.random.dirichlet(alpha=np.ones(n_observations) * 0.5)
        observation_matrix[s, :] = probs
    
    # Generate test data
    test_data = {
        "transition_matrix": transition_matrix,
        "observation_matrix": observation_matrix,
        "states": [f"S{i}" for i in range(n_states)],
        "actions": [f"A{i}" for i in range(n_actions)],
        "observations": [f"O{i}" for i in range(n_observations)]
    }
    
    return test_data

def get_case_definitions() -> Dict[Case, Dict[str, str]]:
    """
    Get definitions for all grammatical cases.
    
    Returns:
        Dictionary mapping Case enum values to case definitions
    """
    return {
        Case.NOMINATIVE: {
            "linguistic_meaning": "Subject/Doer of action",
            "statistical_role": "Active decision-maker",
            "pomdp_context": "The POMDP as an agent actively choosing actions to maximize reward",
            "primary_methods": "get_optimal_action(), transition()"
        },
        Case.ACCUSATIVE: {
            "linguistic_meaning": "Direct object/Receiver of action",
            "statistical_role": "Object of evaluation",
            "pomdp_context": "The POMDP as an object being evaluated or updated by external processes",
            "primary_methods": "evaluate(), update_parameters()"
        },
        Case.DATIVE: {
            "linguistic_meaning": "Indirect object/Recipient",
            "statistical_role": "Recipient of information",
            "pomdp_context": "The POMDP as a recipient of observations or information",
            "primary_methods": "receive_observation(), update_belief()"
        },
        Case.GENITIVE: {
            "linguistic_meaning": "Possession/Source",
            "statistical_role": "Source of information",
            "pomdp_context": "The POMDP as a generative model producing predictions",
            "primary_methods": "predict_next_state(), generate_samples()"
        },
        Case.INSTRUMENTAL: {
            "linguistic_meaning": "Means or instrument",
            "statistical_role": "Tool for decision-making",
            "pomdp_context": "The POMDP as a tool used for policy evaluation and planning",
            "primary_methods": "evaluate_policy(), compute_value()"
        },
        Case.LOCATIVE: {
            "linguistic_meaning": "Location or context",
            "statistical_role": "Environment or context",
            "pomdp_context": "The POMDP as an environment where agents operate",
            "primary_methods": "get_state(), provide_observation()"
        },
        Case.ABLATIVE: {
            "linguistic_meaning": "Origin or source",
            "statistical_role": "Starting point of inference",
            "pomdp_context": "The POMDP as an origin of observations or signals",
            "primary_methods": "generate_observation(), emit_signal()"
        },
        Case.VOCATIVE: {
            "linguistic_meaning": "Direct address",
            "statistical_role": "Addressable entity",
            "pomdp_context": "The POMDP as an entity that can be queried or invoked directly",
            "primary_methods": "respond_to_query(), answer_prompt()"
        }
    }

def run_all_case_tests():
    """Run all grammatical case tests for POMDP models."""
    logger.info("Starting POMDP case tests")
    
    # Generate test data
    test_data = generate_test_data()
    logger.info(f"Generated test data with {len(test_data['states'])} states, " +
               f"{len(test_data['actions'])} actions, {len(test_data['observations'])} observations")
    
    # Get case definitions
    case_definitions = get_case_definitions()
    
    # Run the specifically implemented tests
    logger.info("Running NOMINATIVE case test")
    test_nominative_case(test_data, case_definitions, logger)
    
    logger.info("Running LOCATIVE case test")
    test_locative_case(test_data, case_definitions, logger)
    
    logger.info("Running ABLATIVE case test")
    test_ablative_case(test_data, case_definitions, logger)
    
    logger.info("Running VOCATIVE case test")
    test_vocative_case(test_data, case_definitions, logger)
    
    logger.info("All POMDP case tests completed")

if __name__ == "__main__":
    run_all_case_tests() 