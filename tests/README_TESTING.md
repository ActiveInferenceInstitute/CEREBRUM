# Test-Driven Development in CEREBRUM

This document outlines the test-driven development (TDD) approach used in the CEREBRUM project.

## Overview

CEREBRUM follows test-driven development practices to ensure code quality, maintainability, and correctness. TDD is a development process that relies on the repetition of a very short development cycle:

1. Write a test for the functionality you want to add
2. Run the test and see it fail (since the functionality doesn't exist yet)
3. Write the minimum code to make the test pass
4. Refactor the code to improve quality while keeping the tests passing
5. Repeat

## Testing Structure

CEREBRUM's testing approach is organized into several layers:

### Unit Tests

Unit tests verify individual components in isolation. These tests are fast, focused, and should cover all edge cases for each component. In CEREBRUM, unit tests are organized by component:

- `test_model.py`: Tests for the base Model class
- `test_case_transformations.py`: Tests for case transformation functions
- `test_animal_transformations.py`: Tests for animal-specific transformations
- `test_environment.py`: Tests for environment components

### Integration Tests

Integration tests verify that different components work together correctly. These tests focus on the interactions between components and are located in the `tests/integration/` directory:

- `test_model_interactions.py`: Tests for interactions between models

### Property-Based Tests

Property-based tests use the `hypothesis` framework to test properties that should hold for any valid input, rather than specific examples. This helps find edge cases and unexpected behaviors:

- `test_model_properties.py`: Tests properties of models using randomized inputs

## Running Tests

### Basic Usage

Run all tests:

```bash
python src/scripts/run_tests.py
```

Run unit tests only:

```bash
python src/scripts/run_tests.py --unit-only
```

Run with coverage report:

```bash
python src/scripts/run_tests.py --coverage --html
```

### Advanced Usage

Run tests for a specific module:

```bash
python src/scripts/run_tests.py --module model
```

Run a specific test file:

```bash
pytest tests/core/test_model.py
```

## Test Development Guidelines

### When to Write Tests

- **Before implementation**: Write tests before implementing new features or fixing bugs.
- **For regression**: Add tests when fixing bugs to prevent them from recurring.
- **For refactoring**: Ensure comprehensive test coverage before refactoring.

### Test Quality Checklist

- Tests should be isolated and not depend on other tests.
- Each test should test one thing clearly.
- Tests should have descriptive names explaining what they test.
- Use parameterized tests for similar test cases with different inputs.
- Tests should run quickly and not depend on external resources.

### Best Practices

1. **Arrange-Act-Assert**: Structure tests with setup, execution, and verification phases.
2. **Use fixtures**: Use pytest fixtures for common setup and teardown.
3. **Small, focused tests**: Each test should verify one behavior.
4. **Descriptive test names**: Name tests descriptively, following `test_what_happens_when_condition` pattern.
5. **Test error cases**: Test both success and failure scenarios.
6. **Test edge cases**: Include tests for edge cases and boundary conditions.

## Testing Tools

CEREBRUM uses the following testing tools:

- **pytest**: Core testing framework
- **pytest-cov**: Code coverage reporting
- **hypothesis**: Property-based testing
- **pytest-mock**: Mocking utilities

## Dependencies & Environment

Test dependencies (pytest, pytest-cov, hypothesis, pytest-mock, and the
scientific stack used by the tests) are declared in `pyproject.toml` under the
`dev` extra and installed with uv:

```bash
uv venv
source .venv/bin/activate
uv pip install -e ".[dev]"
```

The project tracks a 90% coverage goal, but the current `pytest.ini` and CI
workflow do not enforce that threshold automatically. To measure coverage, run:

```bash
uv run python -m pytest --cov=src --cov-report=term-missing
```

## Continuous Integration

Tests are automatically run in continuous integration to verify that all tests pass before changes are merged.

## Test Coverage

CEREBRUM aims for high test coverage, especially for core components. Coverage reports can be generated with:

```bash
python src/scripts/run_tests.py --coverage --html
```

The HTML report will be available in the `coverage_html_report` directory.

## Running Linear Regression Tests

### Purpose and Significance

The linear regression tests in CEREBRUM serve as a concrete demonstration of the linguistic case framework applied to statistical modeling. Each linguistic case represents a different perspective or role in the modeling process:

- **Nominative Case (NOM)**: Represents the model as the subject/agent - focuses on the model's parameters and fitting process
- **Accusative Case (ACC)**: Represents the model as the object being evaluated - focuses on model validation and hypothesis testing
- **Dative Case (DAT)**: Represents the model as recipient/processor of data - focuses on data flow and preprocessing
- **Genitive Case (GEN)**: Represents the model as generator of predictions - focuses on model outputs and their interpretation
- **Instrumental Case (INS)**: Represents the model as a tool/method - focuses on algorithmic methodology
- **Locative Case (LOC)**: Represents the model in statistical context - focuses on statistical distributions and assumptions
- **Ablative Case (ABL)**: Represents the model as source of errors - focuses on residual analysis and error origins
- **Vocative Case (VOC)**: Represents the model as interface - focuses on interaction with users or other systems

The visualizations generated by these tests provide intuitive understanding of these different perspectives, making abstract linguistic concepts concrete through statistical visualization.

### Running the Tests

The CEREBRUM framework includes comprehensive linear regression tests that demonstrate the linguistic case framework through statistical modeling. To run these tests and generate visualizations:

```bash
# Run the nominative case test (currently the most reliable)
uv run python -m pytest tests/models/test_linear_regression_comprehensive.py -q
```

The legacy `tests/models/test_linear_regression.py` and its case-specific directory are
excluded by `pytest.ini`; the comprehensive test module above is the maintained
entry point. If you need to run the legacy demonstration manually, treat it as
unsupported and do not interpret its output as CI-verified evidence.

All visualizations and animations will be generated in the `tests/output/linear_regression` directory, organized by case:

```text
tests/output/linear_regression/
├── nom/           # Nominative case visualizations (reliably generated)
├── acc/           # Accusative case visualizations (may have issues)
├── dat/           # Dative case visualizations (may have issues)
├── gen/           # Genitive case visualizations (may have issues)
├── ins/           # Instrumental case visualizations (may have issues)
├── loc/           # Locative case visualizations (may have issues)
├── abl/           # Ablative case visualizations (may have issues)
├── voc/           # Vocative case visualizations (may have issues)
├── cerebrum_cases_overview.png     # Overview of all cases (generated after all tests)
└── cerebrum_cases_simplified.png   # Simplified case overview (generated after all tests)
```

### Linear Regression Case Visualizations

Each case test generates specific visualizations that demonstrate the linguistic case framework in the context of linear regression:

1. **Nominative Case** (Model as active agent/subject):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `active_fitting.png` - Model actively fitting data
   - `gradient_descent_animation.gif` - Animation of the model fitting process
   - `nominative_linguistic.png` - Visual representation of the linguistic structure
   - `nominative_results.txt` - Summary of test results

2. **Accusative Case** (Model as evaluated object):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `cross_validation_animation.gif` - Animation showing model under evaluation
   - `residuals_analysis_animation.gif` - Animation showing residual analysis
   - `accusative_results.txt` - Summary of test results

3. **Dative Case** (Model as data recipient):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `data_flow_animation.gif` - Animation showing data flowing to the model
   - `dative_results.txt` - Summary of test results

4. **Genitive Case** (Model as possessor/source):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `predictions_visualization.png` - Model generating predictions with confidence intervals
   - `intervals_visualization.png` - Summary of prediction intervals
   - `genitive_results.txt` - Summary of test results

5. **Instrumental Case** (Model as tool/method):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `feature_importance_animation.gif` - Animation showing model as analytical instrument
   - `instrumental_results.txt` - Summary of test results

6. **Locative Case** (Model as location/context):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `parameter_space_animation.gif` - Animation showing model in parameter space
   - `locative_results.txt` - Summary of test results

7. **Ablative Case** (Model as error origin):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `error_propagation_animation.gif` - Animation showing errors originating from model
   - `ablative_results.txt` - Summary of test results

8. **Vocative Case** (Model as addressable interface):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `model_component_communication.png` - Model as communication interface
   - `data_addressing_model.png` - Data points addressing the model
   - `model_addressing_data.png` - Model addressing data points
   - `vocative_communication.gif` - Animation of model interactions
   - `vocative_results.txt` - Summary of test results

## Troubleshooting

### Common Test Errors

1. **Missing Dependencies**:
   ```
   ModuleNotFoundError: No module named 'X'
   ```
   Solution: install the project's dev dependencies with `uv pip install -e ".[dev]"`.

2. **Import Errors for Local Modules**:
   ```
   ImportError: No module named 'src'
   ```
   Solution: run pytest from the project root (where `src/` lives), or add the
   project root to `PYTHONPATH`.

3. **Animation Errors**:
   ```
   ValueError: operands could not be broadcast together with shapes (X,) (Y,)
   ```
   Solution: ensure array shapes match in animation functions.

4. **Permission Errors when Writing Output**:
   ```
   PermissionError: [Errno 13] Permission denied: 'path/to/file'
   ```
   Solution: check directory permissions or run with higher privileges.

5. **Coverage Goal Shortfall**: if measured coverage is below the 90% project
   goal, add tests for uncovered paths. The current CI workflow does not fail
   automatically on this threshold.

### Debugging Failed Tests

For failed tests, check:

1. The test logs in the output directory
2. The traceback for the specific error message
3. Generated visualizations for anomalies
4. The state of the model after failure

## References

For more information on testing approaches:

- [pytest documentation](https://docs.pytest.org/)
- [hypothesis documentation](https://hypothesis.readthedocs.io/)
- [Test-Driven Development by Example](https://www.amazon.com/Test-Driven-Development-Kent-Beck/dp/0321146530) by Kent Beck
