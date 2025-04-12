#!/usr/bin/env python
"""
Test runner script for the CEREBRUM project.

This script runs tests with pytest and generates coverage reports.
"""
import os
import sys
import argparse
import subprocess
from pathlib import Path

# Add the project root to the Python path
project_root = Path(__file__).parent.parent.parent
sys.path.append(str(project_root))

def parse_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(description="Run CEREBRUM tests with coverage")
    parser.add_argument(
        "--unit-only", 
        action="store_true", 
        help="Run only unit tests"
    )
    parser.add_argument(
        "--integration-only", 
        action="store_true", 
        help="Run only integration tests"
    )
    parser.add_argument(
        "--coverage", 
        action="store_true", 
        help="Generate coverage report"
    )
    parser.add_argument(
        "--html", 
        action="store_true", 
        help="Generate HTML coverage report"
    )
    parser.add_argument(
        "--module", 
        type=str, 
        help="Run tests for specific module (e.g., model, transformations)"
    )
    parser.add_argument(
        "pytest_args", 
        nargs="*", 
        help="Additional arguments to pass to pytest"
    )
    return parser.parse_args()

def run_tests(args):
    """Run tests with the specified options."""
    # Build pytest command
    cmd = ["python", "-m", "pytest"]
    
    # Add markers for test types
    if args.unit_only:
        cmd.append("-m unit")
    elif args.integration_only:
        cmd.append("-m integration")
    
    # Add module filter if specified
    if args.module:
        cmd.append(f"-m {args.module}")
    
    # Add coverage options
    if args.coverage:
        cmd.append("--cov=src")
        cmd.append("--cov-report=term")
        if args.html:
            cmd.append("--cov-report=html")
    
    # Add any additional pytest arguments
    cmd.extend(args.pytest_args)
    
    # Run the tests
    print(f"Running command: {' '.join(cmd)}")
    return subprocess.run(" ".join(cmd), shell=True, check=False)

def main():
    """Main function."""
    args = parse_args()
    result = run_tests(args)
    sys.exit(result.returncode)

if __name__ == "__main__":
    main() 