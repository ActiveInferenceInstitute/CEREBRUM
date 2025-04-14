#!/usr/bin/env python3
"""
Linear Regression Test Module for CEREBRUM
Testing different linguistic cases in the linear regression context
"""

import os
import sys
import logging
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend for headless environments

# Setup logging
logging.basicConfig(level=logging.INFO, 
                    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger('cerebrum-regression-tests')

# Define required dependencies
REQUIRED_PACKAGES = [
    'pandas', 'numpy', 'matplotlib', 'seaborn', 'scipy', 'sklearn'
]

# Check for required dependencies
missing_packages = []
for package in REQUIRED_PACKAGES:
    try:
        __import__(package)
    except ImportError:
        missing_packages.append(package)

if missing_packages:
    logger.error(f"Missing required packages: {', '.join(missing_packages)}")
    logger.error("Please install required packages using: pip install " + " ".join(missing_packages))
    sys.exit(1)

# Import dependencies
import numpy as np
from models import Case, CaseDefinitions
from utils.data_generator import DataGenerator

# Import all case test functions
from tests.linear_regression_cases import test_dative_case, test_instrumental_case
from tests.linear_regression_cases.vocative_case import test_vocative_case
from tests.linear_regression_cases.genitive_case import test_genitive_case
from tests.linear_regression_cases.locative_case import test_locative_case
from tests.linear_regression_cases.nominative_case import test_nominative_case
from tests.linear_regression_cases.accusative_case import test_accusative_case
from tests.linear_regression_cases.ablative_case import test_ablative_case

# Set up the output directory for visualizations
OUTPUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "output", "linear_regression")
os.makedirs(OUTPUT_DIR, exist_ok=True)

def run_all_case_tests(output_dir: str = OUTPUT_DIR) -> None:
    """Run tests for all linguistic cases in the linear regression context."""
    logger.info("Running all CEREBRUM case tests for LinearRegressionModel")
    
    # Generate test data 
    linear_data = DataGenerator.linear_data(n_samples=100, slope=3.0, intercept=-2.0)
    
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Ensure output directory exists
    os.makedirs(output_dir, exist_ok=True)
    
    # Run tests for all implemented grammatical cases
    # NOMINATIVE case
    if not os.path.exists(os.path.join(output_dir, "nom")) or not os.listdir(os.path.join(output_dir, "nom")):
        logger.info("Running NOMINATIVE case test")
        test_nominative_case(linear_data, output_dir)
    
    # ACCUSATIVE case
    if not os.path.exists(os.path.join(output_dir, "acc")) or not os.listdir(os.path.join(output_dir, "acc")):
        logger.info("Running ACCUSATIVE case test")
        test_accusative_case(linear_data, output_dir)
    
    # DATIVE case
    if not os.path.exists(os.path.join(output_dir, "dat")) or not os.listdir(os.path.join(output_dir, "dat")):
        logger.info("Running DATIVE case test")
        test_dative_case(linear_data, output_dir)
    
    # GENITIVE case
    if not os.path.exists(os.path.join(output_dir, "gen")) or not os.listdir(os.path.join(output_dir, "gen")):
        logger.info("Running GENITIVE case test")
        test_genitive_case(linear_data, output_dir)
    
    # INSTRUMENTAL case
    if not os.path.exists(os.path.join(output_dir, "ins")) or not os.listdir(os.path.join(output_dir, "ins")):
        logger.info("Running INSTRUMENTAL case test")
        test_instrumental_case(linear_data, output_dir)
    
    # LOCATIVE case
    if not os.path.exists(os.path.join(output_dir, "loc")) or not os.listdir(os.path.join(output_dir, "loc")):
        logger.info("Running LOCATIVE case test")
        test_locative_case(linear_data, output_dir)
    
    # ABLATIVE case
    if not os.path.exists(os.path.join(output_dir, "abl")) or not os.listdir(os.path.join(output_dir, "abl")):
        logger.info("Running ABLATIVE case test")
        test_ablative_case(linear_data, output_dir)
    
    # VOCATIVE case
    if not os.path.exists(os.path.join(output_dir, "voc")) or not os.listdir(os.path.join(output_dir, "voc")):
        logger.info("Running VOCATIVE case test")
        test_vocative_case(linear_data, output_dir)
    
    logger.info("Completed all case tests for LinearRegressionModel")

# Run tests if this module is executed directly
if __name__ == "__main__":
    run_all_case_tests()