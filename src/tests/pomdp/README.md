# POMDP Tests

This directory contains tests for the POMDP (Partially Observable Markov Decision Process) implementation in the CEREBRUM framework.

## Directory Structure

- `test_cases/`: Contains test cases for different grammatical cases (nominative, accusative, etc.)
- `utils.py`: Utility functions for POMDP tests
- `visualizers.py`: Visualization utilities for POMDP tests

## Test Cases

The test cases follow the grammatical case model used throughout the CEREBRUM framework:

1. **Nominative Case**: Tests the model as an active agent generating actions/decisions
2. **Accusative Case**: Tests the model as a receiver of actions/updates
3. **Genitive Case**: Tests possession relationships in the model
4. **Dative Case**: Tests the model as an indirect recipient
5. **Instrumental Case**: Tests the model as a means or instrument
6. **Locative Case**: Tests the model in different contexts
7. **Ablative Case**: Tests the model with different origins/sources
8. **Vocative Case**: Tests direct addressing of model components

## Running Tests

To run a specific case test:

```python
from src.tests.pomdp.test_cases.test_nominative import test_nominative_case
from src.core.model import Case

# Define test data and case definitions
pomdp_test_data = {...}  # POMDP test data
case_definitions = {...}  # Case definitions

# Run the test
model = test_nominative_case(pomdp_test_data, case_definitions)
```

## Visualizations

Each test generates visualizations in the `output/pomdp/{case_name}/` directory:

- Linguistic context visualization
- POMDP structure visualization
- Belief state visualizations
- Policy visualizations
- Animations of belief updates

## Adding New Test Cases

To add a new test case:

1. Create a new file in `test_cases/` (e.g., `test_new_case.py`)
2. Implement the test function following the pattern in existing test cases
3. Update any relevant utilities in `utils.py` or `visualizers.py` 