#!/usr/bin/env python3
"""
Animation Fix Script for CEREBRUM
This script will regenerate GIF animations for a specific case file.
"""

import os
import sys
import logging
import importlib
import numpy as np
import argparse
from pathlib import Path

# Set up logging
logging.basicConfig(level=logging.INFO, 
                   format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger("cerebrum-animation-fix")

def setup_paths():
    """Set up Python path to include the project root"""
    # Get the absolute path of the current script
    script_path = os.path.dirname(os.path.abspath(__file__))
    
    # Navigate up to the project root (assuming script is in src/utils)
    project_root = os.path.abspath(os.path.join(script_path, "../.."))
    
    # Add project root to Python path if not already there
    if project_root not in sys.path:
        sys.path.insert(0, project_root)
        logger.info(f"Added {project_root} to Python path")

def generate_test_data():
    """Generate synthetic test data for linear regression"""
    from src.utils.data_generator import DataGenerator
    
    # Create a data generator
    data_gen = DataGenerator()
    
    # Generate linear regression data
    X, y = data_gen.linear_data(n_samples=50, noise_level=1.5)
    
    return X, y

def regenerate_case_animation(case_name):
    """
    Regenerate GIF animations for a specific case
    
    Parameters:
    -----------
    case_name : str
        Name of the case to regenerate animations for (e.g., 'vocative', 'nominative')
    """
    # Check if case name is valid
    valid_cases = [
        'vocative', 'nominative', 'accusative', 'instrumental', 
        'genitive', 'locative', 'ablative', 'dative'
    ]
    
    case_name = case_name.lower()
    if case_name not in valid_cases:
        logger.error(f"Invalid case name: {case_name}. Valid cases are: {', '.join(valid_cases)}")
        return False
    
    # Import the test function for the specified case
    try:
        module_name = f"src.tests.linear_regression_cases.{case_name}_case"
        module = importlib.import_module(module_name)
        test_func = getattr(module, f"test_{case_name}_case")
    except ImportError:
        logger.error(f"Failed to import test function for {case_name} case")
        return False
    except AttributeError:
        logger.error(f"Test function 'test_{case_name}_case' not found in module {module_name}")
        return False
    
    # Generate test data
    X, y = generate_test_data()
    linear_test_data = (X, y)
    
    # Define output directory
    output_dir = os.path.join(
        Path(__file__).parent.parent.parent, 
        "src/tests/output/linear_regression"
    )
    
    # Make sure output directory exists
    os.makedirs(output_dir, exist_ok=True)
    
    # Run the test function to regenerate animations
    logger.info(f"Regenerating animations for {case_name} case")
    try:
        case_output_dir = os.path.join(output_dir, case_name.lower())
        os.makedirs(case_output_dir, exist_ok=True)
        result = test_func(linear_test_data, output_dir)
        logger.info(f"Successfully regenerated animations for {case_name} case")
        return True
    except Exception as e:
        logger.error(f"Error regenerating animations for {case_name} case: {str(e)}")
        return False

def main():
    """Main function to parse arguments and regenerate animations"""
    parser = argparse.ArgumentParser(description="Regenerate GIF animations for linear regression cases")
    parser.add_argument("case", help="Name of the case to regenerate animations for (e.g., vocative, nominative)")
    args = parser.parse_args()
    
    logger.info("Starting animation fix script")
    setup_paths()
    success = regenerate_case_animation(args.case)
    status = "completed successfully" if success else "failed"
    logger.info(f"Animation fix script {status}")

if __name__ == "__main__":
    main() 