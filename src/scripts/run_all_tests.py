#!/usr/bin/env python3
"""
Main script to run all CEREBRUM case tests across model types.

This is a simplified version of test_all_cases.py that directly
calls the main function.
"""

import logging
import os
import sys

# Add the parent directory to path to allow absolute imports
current_dir = os.path.dirname(os.path.abspath(__file__))
parent_dir = os.path.dirname(current_dir)
if parent_dir not in sys.path:
    sys.path.insert(0, parent_dir)

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('cerebrum_tests.log')
    ]
)

logger = logging.getLogger(__name__)

# Import the main test runner function
from src.scripts.test_all_cases import create_output_dir, run_all_tests


def main():
    """Run all case tests and print a summary of the output directories."""
    # Create timestamped output directory
    output_dir = create_output_dir("output")
    logger.info(f"Starting all case tests. Output will be saved to {output_dir}")
    
    # Run all tests
    result_dirs = run_all_tests(output_dir)
    
    # Print summary
    logger.info("All tests completed successfully!")
    logger.info(f"Results available at: {output_dir}")
    logger.info("Directory structure:")
    for model_type, path in result_dirs.items():
        logger.info(f"- {model_type}: {path}")


if __name__ == "__main__":
    main()