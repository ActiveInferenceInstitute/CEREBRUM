import os
import subprocess
import logging
import sys
from datetime import datetime

# --- Configuration ---
# Assuming this script is in src/scripts/, project root is one level up
project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
tests_dir = os.path.join(project_root, "tests")
example_output_dir = os.path.join(tests_dir, "example_outputs") # Define for logging
os.makedirs(example_output_dir, exist_ok=True) # Ensure it exists

# Configure logging
log_filename = os.path.join(tests_dir, f"example_run_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log") # Log inside tests dir
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[
        logging.FileHandler(log_filename),
        logging.StreamHandler(sys.stdout)
    ]
)

python_executable = sys.executable

example_modules = [
    "src.examples.environment",
    "src.examples.animal_visualization",
    "src.examples.streamlined_animal_agent",
    "src.examples.animal_agent",
    "src.examples.hvac_system",
    "src.examples.thermostat"
]

# --- Execution ---
def run_example(module_name):
    """Runs a single example script as a module."""
    logging.info(f"--- Running example: {module_name} ---")
    command = [python_executable, "-m", module_name]
    
    # Set environment variable for PYTHONPATH to include project root
    env = os.environ.copy()
    # Add project_root to the beginning of PYTHONPATH, creating it if it doesn't exist
    env["PYTHONPATH"] = project_root + os.pathsep + env.get("PYTHONPATH", "")
    
    try:
        # Run from project root to ensure modules are found
        result = subprocess.run(
            command,
            cwd=project_root, 
            capture_output=True,
            text=True,
            check=False,
            env=env # Pass modified environment with updated PYTHONPATH
        )
        
        if result.stdout:
            logging.info(f"Output from {module_name}:\n{result.stdout.strip()}")
        
        if result.stderr:
            logging.error(f"Errors from {module_name}:\n{result.stderr.strip()}")
            
        logging.info(f"--- Finished {module_name} with exit code {result.returncode} ---\n")
        return result.returncode
        
    except Exception as e:
        logging.error(f"Failed to run {module_name}: {e}")
        return -1

if __name__ == "__main__":
    logging.info("Starting execution of all examples.")
    logging.info(f"Runner log file: {log_filename}")
    logging.info(f"Example plot outputs will be saved in: {example_output_dir}")
    failed_examples = []
    
    for module in example_modules:
        exit_code = run_example(module)
        if exit_code != 0:
            failed_examples.append(module)
            
    logging.info("--- Summary ---")
    if not failed_examples:
        logging.info("All examples ran successfully.")
    else:
        logging.warning(f"The following examples failed (exit code != 0): {', '.join(failed_examples)}")
        logging.warning("Check the log file for details.")
    
    logging.info(f"Log file saved to: {log_filename}") 