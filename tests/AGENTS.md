# Tests Module Context

## Purpose

Automated verification suite for every CEREBRUM `src/` module. Test subdirectories mirror source modules 1:1 for discoverability.

## Structure

```text
tests/
├── conftest.py                  # Root fixtures (shared by all modules)
├── test_utils.py                # Shared assertion helpers
├── data_utils.py                # Shared data utilities
├── __init__.py
├── core/                        # ← mirrors src/core/
│   ├── test_model.py            #   Model & Case tests
│   ├── test_model_properties.py #   Property-based tests (hypothesis)
│   ├── test_model_registry.py   #   ModelRegistry tests
│   ├── test_active_inference_pomdp.py  # ActiveInferenceModel tests
│   ├── test_neural_network.py   #   NeuralNetworkModel tests
│   └── neural_network/          #   NN test helpers
├── cases/                       # ← mirrors src/cases/
│   └── test_case_manager.py     #   CaseManager tests
├── transformations/             # ← mirrors src/transformations/
│   ├── test_case_transformations.py          # Core transformations
│   ├── test_case_transformations_parametrized.py  # Parametrized suite
│   └── test_animal_transformations.py        # Animal transformations
├── models/                      # ← mirrors src/models/
│   ├── test_insect_models.py    #   Insect model tests
│   ├── test_linear_regression.py #  LinearRegressionModel tests
│   └── linear_regression_cases/ #   Per-case test data/helpers
├── utils/                       # ← mirrors src/utils/
│   ├── test_data_generator.py   #   DataGenerator tests
│   ├── test_array_utils.py      #   Array utility tests
│   └── test_path_utils.py       #   Path utility tests
├── analysis/                    # ← mirrors src/analysis/
│   └── test_analysis.py         #   SimulationEffectivenessAnalyzer tests
├── llm/                         # ← mirrors src/llm/
│   ├── test_ollama.py           #   Ollama client tests
│   ├── test_openrouter_functionality.py  # OpenRouter function tests
│   └── test_openrouter_structure.py      # OpenRouter structure tests
├── examples/                    # ← mirrors src/examples/
│   └── test_environment.py      #   Environment tests
├── integration/                 # Cross-module integration tests
│   └── test_model_interactions.py
├── pomdp/                       # POMDP test infrastructure
│   ├── test_cases/              #   Per-case POMDP tests
│   ├── utils/                   #   POMDP test utilities
│   └── visualizers.py           #   POMDP visualizers
├── data/                        # Test data files
├── output/                      # Test output artifacts
└── example_outputs/             # Example output artifacts
```

## Running Tests

```bash
# All tests (253)
python -m pytest tests/ -v

# By module
python -m pytest tests/core/ -v
python -m pytest tests/transformations/ -v
python -m pytest tests/models/ -v
python -m pytest tests/utils/ -v
python -m pytest tests/llm/ -v

# With coverage
python -m pytest tests/ --cov=src --cov-report=html
```

## Guidelines

1. **No mock methods** — Use real implementations
2. **Mirror src/ structure** — Test files go in the matching subdir
3. **Shared fixtures** in `conftest.py`, shared helpers in `test_utils.py`
4. **Fixed seeds** for reproducibility
5. **Zero warnings** target
