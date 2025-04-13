#!/usr/bin/env python
"""
Test runner script for the CEREBRUM project.

This script runs tests with pytest and generates coverage reports.
"""
import os
import sys
import argparse
import subprocess
import datetime
from pathlib import Path

# Add the project root to the Python path
project_root = Path(__file__).parent.parent.parent
sys.path.append(str(project_root))

# Define test output directory
TEST_OUTPUT_DIR = project_root / "src" / "tests" / "output"

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
        "--output-dir",
        type=str,
        default=str(TEST_OUTPUT_DIR),
        help="Directory to save test output reports"
    )
    parser.add_argument(
        "pytest_args", 
        nargs="*", 
        help="Additional arguments to pass to pytest"
    )
    return parser.parse_args()

def ensure_output_dir(output_dir):
    """Ensure the output directory exists."""
    os.makedirs(output_dir, exist_ok=True)
    return output_dir

def get_timestamp():
    """Get a timestamp string for report filenames."""
    return datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

def run_tests(args):
    """Run tests with the specified options."""
    # Ensure output directory exists
    output_dir = ensure_output_dir(args.output_dir)
    
    # Create timestamped report names
    timestamp = get_timestamp()
    test_type = "unit" if args.unit_only else "integration" if args.integration_only else "all"
    module_str = f"_{args.module}" if args.module else ""
    report_prefix = f"{test_type}{module_str}_{timestamp}"
    
    junit_report = str(Path(output_dir) / f"{report_prefix}_junit.xml")
    log_report = str(Path(output_dir) / f"{report_prefix}_log.txt")
    html_report_dir = str(Path(output_dir) / f"{report_prefix}_html")
    coverage_html_dir = str(Path(output_dir) / f"{report_prefix}_coverage_html")
    
    # Build pytest command
    cmd = ["python3", "-m", "pytest"]
    
    # Add markers for test types
    if args.unit_only:
        cmd.append("-m unit")
    elif args.integration_only:
        cmd.append("-m integration")
    
    # Add module filter if specified
    if args.module:
        cmd.append(f"-m {args.module}")
    
    # Add output reports
    cmd.append(f"--junitxml={junit_report}")
    cmd.append(f"--html={html_report_dir}/report.html")
    cmd.append("--self-contained-html")
    
    # Add coverage options
    if args.coverage:
        cmd.append("--cov=src")
        cmd.append("--cov-report=term")
        cmd.append(f"--cov-report=html:{coverage_html_dir}")
    
    # Add any additional pytest arguments
    cmd.extend(args.pytest_args)
    
    # Run the tests and capture output
    print(f"Running command: {' '.join(cmd)}")
    
    with open(log_report, 'w') as log_file:
        log_file.write(f"CEREBRUM Test Run - {datetime.datetime.now().isoformat()}\n")
        log_file.write(f"Command: {' '.join(cmd)}\n\n")
        log_file.write("-" * 80 + "\n\n")
        
        result = subprocess.run(
            " ".join(cmd), 
            shell=True, 
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True
        )
        
        log_file.write(result.stdout)
        log_file.write(f"\nExit code: {result.returncode}\n")
    
    # Generate a summary report
    generate_summary_report(result, output_dir, report_prefix, args)
    
    # Print test results summary to console
    print(f"\nTests completed with exit code: {result.returncode}")
    print(f"Output saved to: {output_dir}")
    print(f"Log report: {log_report}")
    
    return result

def generate_summary_report(test_result, output_dir, report_prefix, args):
    """Generate a summary report of the test run."""
    summary_report = os.path.join(output_dir, f"{report_prefix}_summary.txt")
    
    with open(summary_report, 'w') as f:
        f.write("CEREBRUM Test Run Summary\n")
        f.write("========================\n\n")
        f.write(f"Date: {datetime.datetime.now().isoformat()}\n")
        f.write(f"Test Type: {'Unit' if args.unit_only else 'Integration' if args.integration_only else 'All'}\n")
        if args.module:
            f.write(f"Module Filter: {args.module}\n")
        f.write(f"Coverage Report: {'Yes' if args.coverage else 'No'}\n")
        f.write(f"HTML Report: {'Yes' if args.html else 'No'}\n")
        f.write(f"Exit Code: {test_result.returncode}\n\n")
        f.write("Test reports available in:\n")
        f.write(f"- {output_dir}\n\n")
        
        # Extract key metrics from test output if possible
        output = test_result.stdout
        
        # Try to find test summary
        try:
            summary_lines = []
            collecting = False
            for line in output.splitlines():
                if "==== " in line and "passed" in line and "failed" in line:
                    summary_lines.append(line)
                    collecting = True
                elif collecting and "==== " in line:
                    summary_lines.append(line)
                elif collecting and line.strip() == "":
                    collecting = False
            
            if summary_lines:
                f.write("Test Summary:\n")
                for line in summary_lines:
                    f.write(f"{line}\n")
                f.write("\n")
                
        except Exception as e:
            f.write(f"Error extracting test summary: {str(e)}\n")
        
        # Try to extract coverage summary
        if args.coverage:
            try:
                coverage_lines = []
                collecting = False
                for line in output.splitlines():
                    if "Coverage report:" in line:
                        collecting = True
                    elif collecting and "-----" in line:
                        coverage_lines.append(line)
                    elif collecting and "TOTAL" in line:
                        coverage_lines.append(line)
                        collecting = False
                
                if coverage_lines:
                    f.write("Coverage Summary:\n")
                    for line in coverage_lines:
                        f.write(f"{line}\n")
                    f.write("\n")
                    
            except Exception as e:
                f.write(f"Error extracting coverage summary: {str(e)}\n")
        
        f.write("\nEnd of Summary Report\n")
    
    return summary_report

def main():
    """Main function."""
    args = parse_args()
    result = run_tests(args)
    sys.exit(result.returncode)

if __name__ == "__main__":
    main() 