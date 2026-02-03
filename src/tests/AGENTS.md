# Tests Module Context

## Purpose

Automated verification suite ensuring correctness, consistency, and performance of the CEREBRUM framework.

## Running Tests

```bash
# Run all tests
python -m pytest src/tests/ -v

# Run specific module tests
python -m pytest src/tests/test_model.py -v

# Run with coverage
python -m pytest src/tests/ --cov=src --cov-report=html
```

## Test Files (253 tests total)

| File | Purpose | Approx Tests |
| ---- | ------- | ------------ |
| `test_model.py` | Base Model and Case tests | ~25 |
| `test_active_inference.py` | Active inference tests | ~30 |
| `test_active_inference_pomdp.py` | POMDP integration tests | ~40 |
| `test_neural_network.py` | Neural network tests | ~35 |
| `test_data_generator.py` | Data utility tests | ~32 |
| `test_case_manager.py` | Case management tests | ~15 |
| `test_transformations.py` | Case transformation tests | ~20 |
| `test_visualization.py` | Visualization tests | ~25 |
| `test_insect_*.py` | Insect model tests | ~30 |

## Test Guidelines

1. **No Mock Methods**: Use real methods, never mock implementations
2. **Comprehensive Coverage**: Test all public methods
3. **Edge Cases**: Include boundary conditions
4. **Warnings-Free**: Tests should produce no warnings
5. **Reproducibility**: Use fixed random seeds where applicable

## New Test Coverage

Recent additions include tests for:

- `Model.get_state()`
- `Model.set_parameters()`
- `Model.get_case_history()`
- `Model.predict()`
- `DataGenerator.classification_data()`
- `DataGenerator.multivariate_data()`
- `DataGenerator.time_series_data()`
- `DataGenerator.split_data()`

## Utilities

See `test_utils.py` for shared test utilities and fixtures.
