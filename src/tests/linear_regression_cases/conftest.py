import pytest
import os
import shutil
from pathlib import Path

@pytest.fixture(scope="session")
def output_dir(project_root):
    """Creates a directory for test outputs for linear regression cases."""
    dir_path = project_root / "src" / "tests" / "output" / "linear_regression"
    # Clean up previous test runs
    if dir_path.exists():
        shutil.rmtree(dir_path)
    dir_path.mkdir(parents=True, exist_ok=True)
    return dir_path 