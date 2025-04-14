import os
import numpy as np
import matplotlib
# Set matplotlib backend to a non-interactive one for test environment
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import pytest
import logging
import time
from typing import Dict, List, Any, Optional, Tuple, Union

from src.core.model import Case
from src.core.neural_network import NeuralNetworkModel

# Import modules from refactored structure
from src.tests.neural_network.case_definitions import CaseDefinitions
from src.tests.neural_network.data_generators import DataGenerator
from src.tests.neural_network.visualizers import create_overview_visualization
from src.tests.neural_network.utils import OUTPUT_DIR, setup_logger, ensure_directory

# Import test case functions
from src.tests.neural_network.test_cases.test_nominative import test_nominative_case
# TODO: Import other test case functions as they are implemented

# Configure logging
logger = setup_logger("__main__")

# Ensure output directory exists
ensure_directory(OUTPUT_DIR)

# Fixtures
@pytest.fixture
def case_definitions():
    """Fixture for case definitions."""
    return CaseDefinitions.get_all_cases()

@pytest.fixture
def nn_regression_data():
    """Fixture for neural network regression test data."""
    return DataGenerator.regression_data(n_samples=200, input_dim=1, output_dim=1)

@pytest.fixture
def nn_classification_data():
    """Fixture for neural network classification test data."""
    return DataGenerator.classification_data(n_samples=200, n_classes=2, n_features=2)

def run_all_case_tests():
    """Run tests for all grammatical cases in neural network context."""
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Generate test data
    regression_data = DataGenerator.regression_data()
    classification_data = DataGenerator.classification_data()
    
    # Dictionary to store models from each case test
    case_models = {}
    
    # Run test for NOMINATIVE case
    logger.info("Running test for NOM case")
    try:
        nom_model = test_nominative_case(regression_data, case_definitions, logger)
        case_models[Case.NOMINATIVE] = nom_model
    except Exception as e:
        logger.error(f"Error in NOM case test: {e}")
    
    # Run test for ACCUSATIVE case
    logger.info("Running test for ACC case")
    try:
        # Placeholder for ACCUSATIVE case test
        # acc_model = test_accusative_case(regression_data, case_definitions, logger)
        # case_models[Case.ACCUSATIVE] = acc_model
        pass  # Remove this when the function is implemented
    except Exception as e:
        logger.error(f"Error in ACC case test: {e}")
    
    # Run test for DATIVE case
    logger.info("Running test for DAT case")
    try:
        # Placeholder for DATIVE case test
        # dat_model = test_dative_case(regression_data, case_definitions, logger)
        # case_models[Case.DATIVE] = dat_model
        pass  # Remove this when the function is implemented
    except Exception as e:
        logger.error(f"Error in DAT case test: {e}")
    
    # Run test for GENITIVE case
    logger.info("Running test for GEN case")
    try:
        # Placeholder for GENITIVE case test
        # gen_model = test_genitive_case(regression_data, case_definitions, logger)
        # case_models[Case.GENITIVE] = gen_model
        pass  # Remove this when the function is implemented
    except Exception as e:
        logger.error(f"Error in GEN case test: {e}")
    
    # Run test for INSTRUMENTAL case
    logger.info("Running test for INS case")
    try:
        # Placeholder for INSTRUMENTAL case test
        # ins_model = test_instrumental_case(regression_data, case_definitions, logger)
        # case_models[Case.INSTRUMENTAL] = ins_model
        pass  # Remove this when the function is implemented
    except Exception as e:
        logger.error(f"Error in INS case test: {e}")
    
    # Run test for LOCATIVE case
    logger.info("Running test for LOC case")
    try:
        # Placeholder for LOCATIVE case test
        # loc_model = test_locative_case(classification_data, case_definitions, logger)
        # case_models[Case.LOCATIVE] = loc_model
        pass  # Remove this when the function is implemented
    except Exception as e:
        logger.error(f"Error in LOC case test: {e}")
    
    # Run test for ABLATIVE case
    logger.info("Running test for ABL case")
    try:
        # Placeholder for ABLATIVE case test
        # abl_model = test_ablative_case(regression_data, case_definitions, logger)
        # case_models[Case.ABLATIVE] = abl_model
        pass  # Remove this when the function is implemented
    except Exception as e:
        logger.error(f"Error in ABL case test: {e}")
    
    # Run test for VOCATIVE case
    logger.info("Running test for VOC case")
    try:
        # Placeholder for VOCATIVE case test
        # voc_model = test_vocative_case(classification_data, case_definitions, logger)
        # case_models[Case.VOCATIVE] = voc_model
        pass  # Remove this when the function is implemented
    except Exception as e:
        logger.error(f"Error in VOC case test: {e}")
    
    # Create overview visualization
    overview_path = os.path.join(OUTPUT_DIR, "neural_network_cases_overview.png")
    create_overview_visualization(save_path=overview_path, logger=logger)
    logger.info(f"All Neural Network case tests completed. Overview available at {overview_path}")
    
    return case_models

if __name__ == "__main__":
    run_all_case_tests() 