#!/usr/bin/env python3
"""
Comprehensive test runner for all CEREBRUM case tests across model types.

This script runs all case tests for all model types (POMDP, Neural Network,
and Linear Regression) and generates comparative visualizations.
"""

import os
import sys
import logging
import argparse
from typing import Dict, List, Optional
import matplotlib.pyplot as plt
from datetime import datetime

# Add the parent directory to path to allow absolute imports
current_dir = os.path.dirname(os.path.abspath(__file__))
parent_dir = os.path.dirname(current_dir)
if parent_dir not in sys.path:
    sys.path.insert(0, parent_dir)

from src.core.model import Model, Case
from src.tests.test_active_inference_pomdp import run_all_case_tests as run_pomdp_tests
from src.tests.test_neural_network import run_all_case_tests as run_nn_tests
from src.tests.test_linear_regression import run_all_case_tests as run_linear_tests
from src.visualization.case_comparison import CaseComparisonVisualizer

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

def create_output_dir(base_dir: str = "output") -> str:
    """Create timestamped output directory."""
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_dir = os.path.join(base_dir, f"all_cases_{timestamp}")
    os.makedirs(output_dir, exist_ok=True)
    return output_dir


def run_all_tests(output_dir: str, specific_case: Optional[str] = None) -> Dict[str, str]:
    """
    Run all case tests across all model types.
    
    Args:
        output_dir: Directory to save outputs
        specific_case: Only run tests for this case if specified
        
    Returns:
        Dictionary mapping test names to output directories
    """
    # Create model-specific output directories
    pomdp_dir = os.path.join(output_dir, "pomdp")
    nn_dir = os.path.join(output_dir, "neural_network")
    linear_dir = os.path.join(output_dir, "linear_regression")
    comparison_dir = os.path.join(output_dir, "comparisons")
    
    for dir_path in [pomdp_dir, nn_dir, linear_dir, comparison_dir]:
        os.makedirs(dir_path, exist_ok=True)
    
    # Run all tests for each model type
    logger.info("Starting POMDP case tests...")
    try:
        pomdp_model_dict = run_pomdp_tests(pomdp_dir, specific_case)
    except TypeError:
        # Try with single argument if it doesn't accept specific_case
        pomdp_model_dict = run_pomdp_tests(pomdp_dir)
    
    logger.info("Starting Neural Network case tests...")
    try:
        nn_model_dict = run_nn_tests(nn_dir, specific_case)
    except TypeError:
        # Try with single argument if it doesn't accept specific_case
        nn_model_dict = run_nn_tests(nn_dir)
    
    logger.info("Starting Linear Regression case tests...")
    try:
        linear_model_dict = run_linear_tests(linear_dir, specific_case)
    except TypeError:
        # Try with single argument if it doesn't accept specific_case
        linear_model_dict = run_linear_tests(linear_dir)
    
    # Create comparison visualizations
    logger.info("Generating cross-model comparisons...")
    comparison_paths = CaseComparisonVisualizer.create_all_case_comparisons(
        output_dir=comparison_dir,
        pomdp_models=pomdp_model_dict,
        nn_models=nn_model_dict,
        linear_models=linear_model_dict
    )
    
    # Generate main index visualization
    logger.info("Creating main index visualization...")
    create_index_visualization(
        output_dir=output_dir,
        pomdp_dir=pomdp_dir,
        nn_dir=nn_dir,
        linear_dir=linear_dir,
        comparison_dir=comparison_dir
    )
    
    # Report results
    logger.info(f"All tests completed successfully.")
    logger.info(f"Output available at: {output_dir}")
    
    return {
        "pomdp": pomdp_dir,
        "neural_network": nn_dir,
        "linear_regression": linear_dir,
        "comparisons": comparison_dir
    }


def create_index_visualization(
    output_dir: str,
    pomdp_dir: str,
    nn_dir: str,
    linear_dir: str,
    comparison_dir: str
) -> str:
    """Create an index visualization showing test results across all models."""
    index_path = os.path.join(output_dir, "index.png")
    
    fig, axs = plt.subplots(2, 2, figsize=(16, 12))
    axs = axs.flatten()
    
    # Title for each subplot
    titles = [
        "POMDP Models",
        "Neural Network Models", 
        "Linear Regression Models",
        "Cross-Model Comparisons"
    ]
    
    dirs = [pomdp_dir, nn_dir, linear_dir, comparison_dir]
    
    for i, (title, dir_path) in enumerate(zip(titles, dirs)):
        ax = axs[i]
        ax.set_title(title, fontsize=14)
        
        # Count case implementations with visualization
        case_count = 0
        for case in Case:
            case_dir = os.path.join(dir_path, case.value.lower())
            if os.path.exists(case_dir) and os.listdir(case_dir):
                case_count += 1
        
        # Create a pie chart showing completed cases
        ax.pie(
            [case_count, len(Case) - case_count],
            labels=[f"{case_count} Cases Implemented", f"{len(Case) - case_count} Cases Pending"],
            autopct='%1.1f%%',
            colors=['green', 'lightgray'],
            startangle=90
        )
        
        # Add case names
        implemented_cases = []
        for case in Case:
            case_dir = os.path.join(dir_path, case.value.lower())
            if os.path.exists(case_dir) and os.listdir(case_dir):
                implemented_cases.append(case.value)
        
        if implemented_cases:
            ax.text(
                0, -1.2,
                f"Implemented: {', '.join(implemented_cases)}",
                ha='center',
                fontsize=10
            )
    
    # Add main title
    fig.suptitle("CEREBRUM Case Implementation Status", fontsize=18)
    
    # Add timestamp
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    fig.text(0.5, 0.01, f"Generated: {timestamp}", ha='center', fontsize=10)
    
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    fig.savefig(index_path, dpi=300)
    plt.close(fig)
    
    return index_path


def main():
    """Main function to parse arguments and run tests."""
    parser = argparse.ArgumentParser(description="Run all CEREBRUM case tests.")
    parser.add_argument("--output-dir", type=str, default="output",
                       help="Base directory for test outputs")
    parser.add_argument("--case", type=str, default=None,
                       help="Run tests only for a specific case (e.g., NOMINATIVE)")
    
    args = parser.parse_args()
    
    # Create output directory
    output_dir = create_output_dir(args.output_dir)
    
    # Run all tests
    run_all_tests(output_dir, args.case)
    
    logger.info(f"All tests completed. Results available at: {output_dir}")


if __name__ == "__main__":
    main() 