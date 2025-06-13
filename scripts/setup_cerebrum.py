#!/usr/bin/env python3
"""
CEREBRUM Setup and Initialization Script
Provides quick setup, testing, and validation of the CEREBRUM framework
"""

import os
import sys
import subprocess
import logging
from pathlib import Path
from typing import List, Dict, Any
import argparse

def setup_logging(verbose: bool = False) -> logging.Logger:
    """Setup logging configuration."""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    return logging.getLogger('cerebrum-setup')

def check_python_version() -> bool:
    """Check if Python version is compatible."""
    version = sys.version_info
    if version.major < 3 or (version.major == 3 and version.minor < 8):
        print(f"❌ Python {version.major}.{version.minor} is not supported. Please use Python 3.8+")
        return False
    print(f"✅ Python {version.major}.{version.minor}.{version.micro} is compatible")
    return True

def check_dependencies() -> bool:
    """Check if required dependencies are installed."""
    required_packages = [
        'numpy', 'scipy', 'pandas', 'matplotlib', 'seaborn', 'sklearn'
    ]
    
    missing = []
    for package in required_packages:
        try:
            __import__(package)
            print(f"✅ {package} is installed")
        except ImportError:
            missing.append(package)
            print(f"❌ {package} is missing")
    
    if missing:
        print(f"\n📦 Install missing packages with: pip install {' '.join(missing)}")
        return False
    
    return True

def install_dependencies(dev: bool = False) -> bool:
    """Install project dependencies."""
    logger = logging.getLogger('cerebrum-setup')
    
    try:
        # Install production dependencies
        logger.info("Installing production dependencies...")
        subprocess.run([sys.executable, '-m', 'pip', 'install', '-r', 'requirements.txt'], 
                      check=True, capture_output=True)
        print("✅ Production dependencies installed")
        
        if dev:
            # Install development dependencies
            logger.info("Installing development dependencies...")
            subprocess.run([sys.executable, '-m', 'pip', 'install', '-r', 'requirements_dev.txt'], 
                          check=True, capture_output=True)
            print("✅ Development dependencies installed")
        
        return True
        
    except subprocess.CalledProcessError as e:
        logger.error(f"Failed to install dependencies: {e}")
        print("❌ Failed to install dependencies")
        return False

def setup_pre_commit() -> bool:
    """Setup pre-commit hooks."""
    logger = logging.getLogger('cerebrum-setup')
    
    try:
        logger.info("Installing pre-commit hooks...")
        subprocess.run([sys.executable, '-m', 'pre_commit', 'install'], 
                      check=True, capture_output=True)
        print("✅ Pre-commit hooks installed")
        return True
        
    except subprocess.CalledProcessError as e:
        logger.error(f"Failed to setup pre-commit: {e}")
        print("⚠️  Pre-commit setup failed (install with: pip install pre-commit)")
        return False

def run_basic_tests() -> bool:
    """Run basic test suite."""
    logger = logging.getLogger('cerebrum-setup')
    
    try:
        logger.info("Running basic tests...")
        
        # Test array utilities
        result = subprocess.run([
            sys.executable, '-c', 
            'from src.utils.array_utils import validate_regression_data; '
            'import numpy as np; '
            'X, y = validate_regression_data(np.random.rand(10, 1), np.random.rand(10)); '
            'print("Array utils working")'
        ], check=True, capture_output=True, text=True)
        
        # Test data generator
        result = subprocess.run([
            sys.executable, '-c',
            'from src.utils.data_generator import DataGenerator; '
            'X, y = DataGenerator.linear_data(n_samples=10); '
            'print("Data generator working")'
        ], check=True, capture_output=True, text=True)
        
        # Test model registry
        result = subprocess.run([
            sys.executable, '-c',
            'from src.core.model_registry import get_global_registry; '
            'registry = get_global_registry(); '
            'print("Model registry working")'
        ], check=True, capture_output=True, text=True)
        
        print("✅ Basic functionality tests passed")
        return True
        
    except subprocess.CalledProcessError as e:
        logger.error(f"Basic tests failed: {e}")
        print("❌ Basic functionality tests failed")
        return False

def run_quick_model_test() -> bool:
    """Run a quick model test."""
    logger = logging.getLogger('cerebrum-setup')
    
    try:
        logger.info("Running quick model test...")
        
        test_script = '''
from src.models.linear_regression import LinearRegressionModel
from src.models.base import Case
from src.utils.data_generator import DataGenerator
from src.core.model_registry import register_model
import numpy as np

# Generate test data
X, y = DataGenerator.linear_data(n_samples=50, random_seed=42)

# Create and fit model
model = LinearRegressionModel("test_model", Case.NOMINATIVE)
model.fit(X, y)

# Make predictions
y_pred = model.predict(X)

# Register model
model_id = register_model(model, tags=["test"], description="Quick test model")

print(f"✅ Model test successful - R² = {model.evaluate(X, y)['r2']:.4f}")
print(f"✅ Model registered as: {model_id}")
'''
        
        result = subprocess.run([sys.executable, '-c', test_script], 
                              check=True, capture_output=True, text=True)
        print(result.stdout.strip())
        return True
        
    except subprocess.CalledProcessError as e:
        logger.error(f"Model test failed: {e}")
        print("❌ Model test failed")
        if e.stderr:
            print(f"Error: {e.stderr}")
        return False

def create_output_directories() -> bool:
    """Create necessary output directories."""
    directories = [
        'output',
        'output/model_registry',
        'output/test_results',
        'src/tests/output'
    ]
    
    for dir_path in directories:
        Path(dir_path).mkdir(parents=True, exist_ok=True)
        print(f"✅ Created directory: {dir_path}")
    
    return True

def validate_project_structure() -> bool:
    """Validate that the project structure is correct."""
    required_files = [
        'src/__init__.py',
        'src/models/__init__.py',
        'src/core/__init__.py',
        'src/utils/__init__.py',
        'requirements.txt',
        'requirements_dev.txt',
        'pytest.ini'
    ]
    
    missing_files = []
    for file_path in required_files:
        if not Path(file_path).exists():
            missing_files.append(file_path)
            print(f"❌ Missing: {file_path}")
        else:
            print(f"✅ Found: {file_path}")
    
    if missing_files:
        print(f"\n⚠️  Missing {len(missing_files)} required files")
        return False
    
    return True

def run_setup(args: argparse.Namespace) -> bool:
    """Run the complete setup process."""
    logger = setup_logging(args.verbose)
    logger.info("Starting CEREBRUM setup process")
    
    print("🧠 CEREBRUM Setup and Validation")
    print("=" * 40)
    
    success = True
    
    # Check Python version
    print("\n1. Checking Python version...")
    if not check_python_version():
        return False
    
    # Validate project structure
    print("\n2. Validating project structure...")
    if not validate_project_structure():
        success = False
    
    # Check dependencies
    print("\n3. Checking dependencies...")
    if not check_dependencies():
        if args.install_deps:
            print("\n📦 Installing dependencies...")
            if not install_dependencies(dev=args.dev):
                return False
        else:
            success = False
    
    # Create output directories
    print("\n4. Creating output directories...")
    create_output_directories()
    
    # Setup pre-commit (if requested)
    if args.setup_precommit:
        print("\n5. Setting up pre-commit hooks...")
        setup_pre_commit()
    
    # Run basic tests
    if args.run_tests:
        print("\n6. Running basic functionality tests...")
        if not run_basic_tests():
            success = False
        
        print("\n7. Running quick model test...")
        if not run_quick_model_test():
            success = False
    
    # Summary
    print("\n" + "=" * 40)
    if success:
        print("🎉 CEREBRUM setup completed successfully!")
        print("\nNext steps:")
        print("  - Run full tests: python -m pytest src/tests/")
        print("  - Try examples: cd src/examples && python linear_regression_example.py")
        print("  - Read docs: docs/getting_started.md")
    else:
        print("❌ Setup completed with issues. Please fix the problems above.")
    
    return success

def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description="CEREBRUM Setup and Validation")
    parser.add_argument('--install-deps', action='store_true', 
                       help='Install dependencies if missing')
    parser.add_argument('--dev', action='store_true',
                       help='Install development dependencies')
    parser.add_argument('--setup-precommit', action='store_true',
                       help='Setup pre-commit hooks')
    parser.add_argument('--run-tests', action='store_true',
                       help='Run basic tests')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Enable verbose logging')
    parser.add_argument('--all', action='store_true',
                       help='Run all setup steps')
    
    args = parser.parse_args()
    
    # If --all is specified, enable all options
    if args.all:
        args.install_deps = True
        args.dev = True
        args.setup_precommit = True
        args.run_tests = True
    
    # Change to project root directory
    project_root = Path(__file__).parent.parent
    os.chdir(project_root)
    
    success = run_setup(args)
    sys.exit(0 if success else 1)

if __name__ == '__main__':
    main() 