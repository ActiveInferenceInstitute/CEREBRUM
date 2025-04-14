#!/usr/bin/env python3
"""
Linear Regression Test Module for CEREBRUM
Testing different linguistic cases in the linear regression context

This module tests linear regression models in the context of linguistic cases.
Each case represents a different perspective or role of the model in the regression process.
Tests generate visualizations, animations, and performance metrics for each case.

Directory Structure:
- output/linear_regression/
  - nominative/    # Model as subject of action (parameter estimator)
  - accusative/    # Model as object being evaluated
  - dative/        # Model as recipient of data
  - genitive/      # Analyzing model's parameters and properties
  - instrumental/  # Model as a tool for analysis
  - locative/      # Position of model in parameter space
  - ablative/      # Model as source of predictions
  - vocative/      # Model as entity being addressed/called upon

Each case directory contains visualizations and animations specific to that case's
linguistic interpretation in the context of linear regression.
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
from src.models import Case, CaseDefinitions
from src.utils.data_generator import DataGenerator

# Import all case test functions
from src.tests.linear_regression_cases import test_dative_case, test_instrumental_case
from src.tests.linear_regression_cases.vocative_case import test_vocative_case
from src.tests.linear_regression_cases.genitive_case import test_genitive_case
from src.tests.linear_regression_cases.locative_case import test_locative_case
from src.tests.linear_regression_cases.nominative_case import test_nominative_case
from src.tests.linear_regression_cases.accusative_case import test_accusative_case
from src.tests.linear_regression_cases.ablative_case import test_ablative_case

# Set up the output directory for visualizations
OUTPUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "output", "linear_regression")
os.makedirs(OUTPUT_DIR, exist_ok=True)

def run_all_case_tests(output_dir: str = OUTPUT_DIR, force_regenerate: bool = True) -> None:
    """Run tests for all linguistic cases in the linear regression context.
    
    This function orchestrates the testing of all linguistic cases applied to 
    linear regression models. For each case, it generates test data, creates
    visualizations, and documents the results in case-specific directories.
    
    The function ensures consistent directory naming by using full case names
    (e.g., "nominative" instead of "nom") and handles any legacy abbreviated directories
    by merging their contents into the full name directories.
    
    Args:
        output_dir: Directory to store output visualizations
        force_regenerate: If True, regenerate all outputs even if they already exist
        
    Returns:
        None
    """
    logger.info("Running all CEREBRUM case tests for LinearRegressionModel")
    
    # Generate test data 
    linear_data = DataGenerator.linear_data(n_samples=100, slope=3.0, intercept=-2.0)
    
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Ensure output directory exists
    os.makedirs(output_dir, exist_ok=True)
    
    # Define case mapping for full names
    case_full_names = {
        Case.NOMINATIVE: "nominative",
        Case.ACCUSATIVE: "accusative",
        Case.DATIVE: "dative",
        Case.GENITIVE: "genitive",
        Case.INSTRUMENTAL: "instrumental",
        Case.LOCATIVE: "locative",
        Case.ABLATIVE: "ablative",
        Case.VOCATIVE: "vocative"
    }
    
    # Cleanup existing abbreviated folders if they exist
    # This code handles legacy directory names and ensures a clean structure
    for abbreviated, full_name in zip(
        ["nom", "acc", "dat", "gen", "ins", "loc", "abl", "voc"],
        ["nominative", "accusative", "dative", "genitive", "instrumental", "locative", "ablative", "vocative"]
    ):
        abbreviated_dir = os.path.join(output_dir, abbreviated)
        full_dir = os.path.join(output_dir, full_name)
        
        # If both abbreviated and full directories exist, merge their contents
        if os.path.exists(abbreviated_dir) and os.path.exists(full_dir):
            logger.info(f"Merging contents from {abbreviated_dir} to {full_dir}")
            # Copy any unique files from abbreviated dir to full dir
            for item in os.listdir(abbreviated_dir):
                src_path = os.path.join(abbreviated_dir, item)
                dst_path = os.path.join(full_dir, item)
                
                if os.path.isfile(src_path) and not os.path.exists(dst_path):
                    import shutil
                    shutil.copy2(src_path, dst_path)
                elif os.path.isdir(src_path) and not os.path.exists(dst_path):
                    import shutil
                    shutil.copytree(src_path, dst_path)
            
            # Remove the abbreviated directory after merging
            import shutil
            shutil.rmtree(abbreviated_dir)
    
    # Run tests for all implemented grammatical cases
    # Each case test function will create visualizations and animations
    # specific to the linguistic interpretation of that case
    
    # NOMINATIVE case - Model as active agent fitting parameters
    if force_regenerate or not os.path.exists(os.path.join(output_dir, case_full_names[Case.NOMINATIVE])) or not os.listdir(os.path.join(output_dir, case_full_names[Case.NOMINATIVE])):
        logger.info("Running NOMINATIVE case test")
        test_nominative_case(linear_data, output_dir)
    
    # ACCUSATIVE case - Model as object being evaluated
    if force_regenerate or not os.path.exists(os.path.join(output_dir, case_full_names[Case.ACCUSATIVE])) or not os.listdir(os.path.join(output_dir, case_full_names[Case.ACCUSATIVE])):
        logger.info("Running ACCUSATIVE case test")
        test_accusative_case(linear_data, output_dir)
    
    # DATIVE case - Model as recipient of data
    if force_regenerate or not os.path.exists(os.path.join(output_dir, case_full_names[Case.DATIVE])) or not os.listdir(os.path.join(output_dir, case_full_names[Case.DATIVE])):
        logger.info("Running DATIVE case test")
        test_dative_case(linear_data, output_dir)
    
    # GENITIVE case - Analyzing model's parameters and properties
    if force_regenerate or not os.path.exists(os.path.join(output_dir, case_full_names[Case.GENITIVE])) or not os.listdir(os.path.join(output_dir, case_full_names[Case.GENITIVE])):
        logger.info("Running GENITIVE case test")
        test_genitive_case(linear_data, output_dir)
    
    # INSTRUMENTAL case - Model as tool for analysis
    if force_regenerate or not os.path.exists(os.path.join(output_dir, case_full_names[Case.INSTRUMENTAL])) or not os.listdir(os.path.join(output_dir, case_full_names[Case.INSTRUMENTAL])):
        logger.info("Running INSTRUMENTAL case test")
        test_instrumental_case(linear_data, output_dir)
    
    # LOCATIVE case - Position of model in parameter space
    if force_regenerate or not os.path.exists(os.path.join(output_dir, case_full_names[Case.LOCATIVE])) or not os.listdir(os.path.join(output_dir, case_full_names[Case.LOCATIVE])):
        logger.info("Running LOCATIVE case test")
        test_locative_case(linear_data, output_dir)
    
    # ABLATIVE case - Model as source of predictions
    if force_regenerate or not os.path.exists(os.path.join(output_dir, case_full_names[Case.ABLATIVE])) or not os.listdir(os.path.join(output_dir, case_full_names[Case.ABLATIVE])):
        logger.info("Running ABLATIVE case test")
        test_ablative_case(linear_data, output_dir)
    
    # VOCATIVE case - Model as entity being addressed
    if force_regenerate or not os.path.exists(os.path.join(output_dir, case_full_names[Case.VOCATIVE])) or not os.listdir(os.path.join(output_dir, case_full_names[Case.VOCATIVE])):
        logger.info("Running VOCATIVE case test")
        test_vocative_case(linear_data, output_dir)
    
    logger.info("Completed all case tests for LinearRegressionModel")
    
    # Document the directory structure
    with open(os.path.join(output_dir, "README.md"), 'w') as f:
        f.write("# Linear Regression Case Tests\n\n")
        f.write("This directory contains test results for linear regression models in the context of linguistic cases.\n\n")
        f.write("## Directory Structure\n\n")
        
        for case, full_name in case_full_names.items():
            case_info = case_definitions[case]
            f.write(f"- **{full_name}/** - {case_info['linguistic_meaning']} in regression context\n")
            f.write(f"  - Statistical role: {case_info['statistical_role']}\n")
            f.write(f"  - Regression context: {case_info.get('regression_context', 'N/A')}\n\n")
        
        f.write("\n## Generated Visualizations\n\n")
        f.write("Each case directory contains visualizations specific to that case's interpretation in linear regression:\n\n")
        f.write("- **linguistic_context.png** - Visual representation of the case's linguistic meaning\n")
        f.write("- Animation files (GIF format) showing the dynamic behavior of the model\n")
        f.write("- Static visualizations of model parameters, predictions, and error metrics\n")

# Run tests if this module is executed directly
if __name__ == "__main__":
    run_all_case_tests()