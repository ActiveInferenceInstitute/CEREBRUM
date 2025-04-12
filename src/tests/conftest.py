import pytest
import os
import sys
import logging
import numpy as np
from pathlib import Path
from typing import Dict, Any

# Add the src directory to the Python path for proper imports
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

# Configure test logger
@pytest.fixture(scope="session")
def logger():
    """Return a configured logger for use in tests."""
    log_format = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    logging.basicConfig(level=logging.DEBUG, format=log_format)
    return logging.getLogger("cerebrum-tests")

# Common model parameters fixtures
@pytest.fixture
def default_model_params() -> Dict[str, Any]:
    """Return default parameters for models."""
    return {
        "learning_rate": 0.01,
        "precision": 1.0,
    }

@pytest.fixture
def default_active_inference_params() -> Dict[str, Any]:
    """Return default parameters for active inference models."""
    return {
        "prior_means": np.zeros(3),
        "prior_precision": np.eye(3),
        "likelihood_precision": np.eye(3),
    }

# Path fixtures
@pytest.fixture(scope="session")
def project_root() -> Path:
    """Return the project root directory as Path object."""
    return Path(__file__).parent.parent.parent

@pytest.fixture(scope="session")
def test_data_dir(project_root) -> Path:
    """Return the test data directory, creating it if it doesn't exist."""
    data_dir = project_root / "tests" / "data"
    data_dir.mkdir(parents=True, exist_ok=True)
    return data_dir 