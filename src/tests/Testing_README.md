# CEREBRUM Testing Framework

This directory contains the testing framework for the CEREBRUM project, following test-driven development principles.

## Testing Approach

The CEREBRUM testing approach follows several key principles:

1. **Test-Driven Development (TDD)**: Tests are written before implementation code.
2. **Comprehensive Coverage**: Aim for high test coverage across all components.
3. **Separation of Concerns**: Unit tests, integration tests, and end-to-end tests are clearly separated.
4. **Automated Testing**: All tests can be run automatically via the test runner.
5. **Regression Prevention**: Tests ensure new changes don't break existing functionality.

## Test Structure

Tests are organized into several categories:

- **Unit Tests**: Test individual components in isolation
- **Integration Tests**: Test interactions between components
- **End-to-End Tests**: Test complete workflows through the system
- **Parametrized Tests**: Test multiple scenarios with a single test function
- **Property-Based Tests**: Test properties that should hold across many inputs

## Directory Structure

```
tests/
├── __init__.py
├── conftest.py               # Common pytest fixtures and configuration
├── test_*.py                 # Test modules for different components
├── test_utils.py             # Utility functions for tests
├── data/                     # Test data directory
└── output/                   # Test reports directory
```

## Running Tests

### Using the Test Runner

The simplest way to run tests is using the test runner script:

```bash
# Run all tests
python3 src/scripts/run_tests.py

# Run only unit tests
python3 src/scripts/run_tests.py --unit-only

# Run tests with coverage
python3 src/scripts/run_tests.py --coverage

# Run tests with coverage and HTML report
python3 src/scripts/run_tests.py --coverage --html

# Run tests for a specific module
python3 src/scripts/run_tests.py --module model
```

### Using pytest Directly

You can also run pytest commands directly:

```bash
# Run all tests
python3 -m pytest

# Run specific test file
python3 -m pytest src/tests/test_model.py

# Run tests matching a pattern
python3 -m pytest -k "model"

# Run tests with specific markers
python3 -m pytest -m "unit and model"

# Run tests with coverage
python3 -m pytest --cov=src
```

## Writing New Tests

When adding new functionality, follow these steps:

1. Create a new test file or add to an existing test file.
2. Write tests for the new functionality following TDD principles.
3. Implement the functionality to make the tests pass.
4. Refactor the code while keeping the tests passing.

### Test Naming Conventions

- Test files: `test_*.py`
- Test classes: `Test*`
- Test functions: `test_*`

### Example Test

```python
import pytest
from src.core.model import Model

@pytest.mark.unit
def test_model_initialization():
    """Test that a model initializes with correct default values."""
    model = Model()
    assert model.name.startswith("Model_")
    assert model.parameters == {}
```

## Fixtures and Utilities

Common fixtures are defined in `conftest.py` and testing utilities in `test_utils.py`.

## Continuous Integration

Tests are automatically run as part of the CI/CD pipeline for every pull request and merge to main.

## Test Reports

When you run the tests, various reports are generated in the `src/tests/output` directory:

```
output/
├── all_YYYYMMDD_HHMMSS_coverage_html/   # HTML coverage report (with --coverage)
├── all_YYYYMMDD_HHMMSS_html/            # HTML test report (includes report.html)
├── all_YYYYMMDD_HHMMSS_junit.xml        # JUnit XML report for CI integration
├── all_YYYYMMDD_HHMMSS_log.txt          # Detailed test logs
└── all_YYYYMMDD_HHMMSS_summary.txt      # Test run summary
```

Where `YYYYMMDD_HHMMSS` is a timestamp of when the tests were run.

### Coverage Reports

To generate a coverage report showing which parts of the code are covered by tests:

```bash
# Generate coverage report with HTML
python3 src/scripts/run_tests.py --coverage --html
```

The HTML coverage report will be available at:
`src/tests/output/all_YYYYMMDD_HHMMSS_coverage_html/index.html`

The main test report can be accessed at:
`src/tests/output/all_YYYYMMDD_HHMMSS_html/report.html` 