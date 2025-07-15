# LEXICON Tests

This directory contains tests for the LEXICON system.

## Overview

The LEXICON test suite includes:

1. **Component Tests**: Tests for individual components of the LEXICON system
2. **End-to-End Tests**: Tests for the complete LEXICON pipeline

## Running Tests

You can run the tests using Python's unittest framework:

```bash
# Run component tests
python3 test_components.py

# Run end-to-end tests
python3 test_end_to_end.py

# Run all tests
python3 -m unittest discover
```

## Test Files

- **test_components.py**: Tests for individual components:
  - Configuration initialization
  - Content ID generator
  - NLP preprocessor
  - Case tagger
  - Entity linker
  - Engine initialization

- **test_end_to_end.py**: Tests for the complete pipeline:
  - Text processing
  - Format parser handling

## Test Output

Test results are saved in the `output/lexicon/` directory with timestamped subdirectories:

```
output/lexicon/test_components_[timestamp]/
output/lexicon/test_end_to_end_[timestamp]/
```

Each test output directory contains:
- JSON results
- Text visualizations
- PNG visualizations (if matplotlib is installed)
- GIF animations (if imageio is installed)

## Requirements

The tests require:
- Python 3.8+
- OpenRouter API key (set as environment variable `OPENROUTER_API_KEY`)
- Optional: matplotlib, networkx, numpy, imageio (for visualizations)

## Adding New Tests

When adding new tests:

1. Follow the unittest framework pattern
2. Use the `setUp` and `tearDown` methods for initialization and cleanup
3. Use descriptive test names starting with `test_`
4. Add appropriate assertions
5. Document the test purpose and expected behavior 