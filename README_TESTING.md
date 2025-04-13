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
pytest src/tests/test_model.py
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
python3 -c "from src.tests.test_linear_regression import CaseDefinitions, DataGenerator, test_nominative_case; case_definitions = CaseDefinitions.get_all_cases(); linear_data = DataGenerator.linear_data(n_samples=150, slope=3.0, intercept=-2.0, noise_level=2.0); test_nominative_case(linear_data, case_definitions)"
```

**Note:** Currently, there are array dimension issues with several of the case tests when run individually. Work is ongoing to fix these issues. The nominative case test is the most reliable and demonstrates the core concepts of the CEREBRUM framework in the context of linear regression.

All visualizations and animations will be generated in the `src/tests/output/linear_regression` directory, organized by case:

```
src/tests/output/linear_regression/
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
   - `fitting_visualization.png` - Model actively fitting data
   - `fitting_animation.gif` - Animation of the model fitting process
   - `parameter_space_visualization.png` - Model exploring parameter space
   - `formula_visualization.png` - The mathematical representation
   - `nominative_results.txt` - Summary of test results

2. **Dative Case** (Model as data recipient):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `data_flow_visualization.png` - Visualization of data flowing to the model
   - `data_processing_visualization.png` - Model processing received data
   - `dative_results.txt` - Summary of test results

3. **Genitive Case** (Model as possessor/source):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `predictions_visualization.png` - Model generating predictions with confidence intervals
   - `summary_visualization.png` - Model as source of information
   - `prediction_generation_animation.gif` - Animation of prediction generation
   - `genitive_results.txt` - Summary of test results

4. **Instrumental Case** (Model as tool/method):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `methodology_visualization.png` - Model as method implementation
   - `algorithm_animation.gif` - Animation of the algorithm process
   - `instrumental_results.txt` - Summary of test results

5. **Locative Case** (Model as location/context):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `statistical_context_visualization.png` - Model as statistical assumption framework
   - `statistical_context_animation.gif` - Animation showing statistical contexts
   - `locative_results.txt` - Summary of test results

6. **Ablative Case** (Model as error origin):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `error_source_visualization.png` - Model as source of errors
   - `error_origin_animation.gif` - Animation showing errors originating from model
   - `ablative_results.txt` - Summary of test results

7. **Vocative Case** (Model as addressable interface):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `interface_visualization.png` - Model as interactive interface
   - `interaction_animation.gif` - Animation of model interactions
   - `vocative_results.txt` - Summary of test results

8. **Accusative Case** (Model as evaluated object):
   - `linguistic_context.png` - Case explanation with linguistic context
   - `evaluation_visualization.png` - Model undergoing evaluation
   - `hypothesis_testing.png` - Hypothesis testing of model significance
   - `evaluation_animation.gif` - Animation of evaluation process (currently has an issue)
   - `accusative_results.txt` - Summary of test results

## References

For more information on testing approaches:

- [pytest documentation](https://docs.pytest.org/)
- [hypothesis documentation](https://hypothesis.readthedocs.io/)
- [Test-Driven Development by Example](https://www.amazon.com/Test-Driven-Development-Kent-Beck/dp/0321146530) by Kent Beck 