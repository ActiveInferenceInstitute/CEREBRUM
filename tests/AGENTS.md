# Tests Module Context

## Purpose

Automated verification suite for every CEREBRUM `src/` module. Test subdirectories mirror source modules 1:1 for discoverability. Additional tests in `beyond_cerebrum/src/tests/` cover the extended calculus operations.

## Statistics

| Metric | Value |
|--------|-------|
| Test files | 67 (65 in `tests/` + 2 in `beyond_cerebrum/src/tests/`) |
| Collected tests | 1207 |
| Passing | 1205 |
| Skipped | 2 (live API, opt-in via `@pytest.mark.live`) |
| Ignored paths | `tests/pomdp/test_cases`, `tests/models/test_linear_regression.py`, `tests/models/linear_regression_cases` |

## Structure

```text
tests/
├── conftest.py                          # Root fixtures (shared by all modules)
├── test_utils.py                        # Shared assertion helpers (ModelHelper, TempFileManager)
├── data_utils.py                        # Shared data utilities
├── __init__.py
├── analysis/                            # ← mirrors src/analysis/
│   ├── test_analysis.py                 #   Analysis tests
│   └── test_simulation_assessment.py    #   SimulationEffectivenessAnalyzer
├── cases/                               # ← mirrors src/cases/
│   ├── test_animal_cases.py             #   AnimalCaseManager tests
│   ├── test_case_default_paths.py       #   Default case path tests
│   ├── test_case_handlers.py            #   Case handler tests
│   ├── test_case_manager.py             #   CaseManager tests
│   ├── test_case_manager_coverage.py    #   CaseManager coverage boost
│   ├── test_cases_examples.py           #   Example functions tests
│   ├── test_edge_cases_extended.py      #   Edge case coverage
│   └── test_individual_cases.py         #   All 8 case handlers (parametrized)
├── core/                                # ← mirrors src/core/
│   ├── test_active_inference_coverage.py #  ActiveInference coverage boost
│   ├── test_active_inference_extended.py #  Extended ActiveInference tests
│   ├── test_active_inference_pomdp.py   #   ActiveInferenceModel POMDP tests
│   ├── test_model.py                    #   Model & Case tests
│   ├── test_model_properties.py         #   Property-based tests (hypothesis)
│   ├── test_model_registry.py           #   ModelRegistry tests
│   ├── test_model_registry_coverage.py  #   ModelRegistry coverage boost
│   ├── test_neural_network.py           #   NeuralNetworkModel tests
│   ├── test_nn_case_updates.py          #   NN case update tests
│   └── neural_network/                  #   NN test helpers
├── examples/                            # ← mirrors src/examples/
│   └── test_environment.py              #   Environment tests
├── integration/                         # Cross-module integration tests
│   └── test_model_interactions.py
├── lexicon/                             # ← mirrors src/lexicon/
│   ├── test_core.py                     #   Config, exceptions, logging
│   ├── test_graph_and_declension.py     #   CID generator, CaseRules
│   └── test_nlp.py                      #   SentenceSplitter
├── llm/                                 # ← mirrors src/llm/
│   ├── test_llm_utils.py               #   LLM utility tests
│   ├── test_openrouter_functionality.py #   OpenRouter function tests
│   ├── test_openrouter_live.py          #   OpenRouter live tests (@pytest.mark.live)
│   └── test_openrouter_structure.py     #   OpenRouter structure tests
├── models/                              # ← mirrors src/models/
│   ├── test_insect_base_extended.py     #   InsectModel & ActiveInference extended
│   ├── test_insect_behaviors.py         #   Insect behavior tests
│   ├── test_insect_cases.py             #   Insect case tests
│   ├── test_insect_cases_extended.py    #   Extended insect case coverage
│   ├── test_insect_models.py            #   Insect model tests
│   ├── test_insect_species.py           #   Species-specific tests
│   ├── test_linear_regression.py        #   LinearRegressionModel (ignored, see comprehensive)
│   ├── test_linear_regression_comprehensive.py  # Full regression test suite
│   └── linear_regression_cases/         #   Per-case test data/helpers (ignored)
├── pomdp/                               # POMDP test infrastructure
│   ├── test_cases/                      #   Per-case POMDP tests (ignored)
│   ├── models/                          #   POMDP model helpers
│   └── utils/                           #   POMDP test utilities
├── transformations/                     # ← mirrors src/transformations/
│   ├── test_animal_transformations.py   #   Animal transformations
│   ├── test_case_transformations.py     #   Core transformations
│   ├── test_case_transformations_extended.py     # Extended coverage
│   └── test_case_transformations_parametrized.py # Parametrized suite
├── utils/                               # ← mirrors src/utils/
│   ├── test_animation_utils.py          #   Animation utility tests
│   ├── test_array_utils.py              #   Array utility tests
│   ├── test_data_generator.py           #   DataGenerator tests
│   ├── test_markdown_utils.py           #   Markdown utility tests
│   ├── test_output_organizer.py         #   SimulationOutputOrganizer tests
│   ├── test_path_utils.py              #   Path utility tests
│   └── test_visualization_utils.py      #   Visualizer, causal mechanisms, animations
├── visualization/                       # ← mirrors src/visualization/
│   ├── test_animal_visualization.py     #   Animal visualization tests
│   ├── test_animation_creator.py        #   InsectAnimationCreator tests
│   ├── test_behavior_visualizer.py      #   BehaviorVisualizer tests
│   ├── test_case_comparison.py          #   CaseComparisonVisualizer tests
│   ├── test_case_comparison_coverage.py #   CaseComparison coverage boost
│   ├── test_case_visualization.py       #   plot_model_state, transitions, ecosystem
│   ├── test_case_visualizer.py          #   CaseVisualizer tests
│   ├── test_comprehensive_visualizer.py #   Comprehensive visualizer tests
│   ├── test_insect_visualizer.py        #   Insect visualizer tests
│   ├── test_neural_visualizer.py        #   NeuralStructureVisualizer tests
│   ├── test_report_generator.py         #   ReportGenerator tests
│   ├── test_simulation_logger.py        #   SimulationLogger tests
│   └── test_visualization_extended.py   #   Extended visualization coverage
├── data/                                #   Test data files
├── output/                              #   Test output artifacts
└── example_outputs/                     #   Example output artifacts
```

## Running Tests

```bash
# All tests (1207)
python -m pytest -v

# By module
python -m pytest tests/core/ -v
python -m pytest tests/cases/ -v
python -m pytest tests/transformations/ -v
python -m pytest tests/models/ -v
python -m pytest tests/utils/ -v
python -m pytest tests/visualization/ -v
python -m pytest tests/llm/ -v
python -m pytest tests/lexicon/ -v
python -m pytest tests/analysis/ -v

# With coverage
python -m pytest --cov=src --cov-report=html

# Live API tests (opt-in, requires OPENROUTER_API_KEY)
OPENROUTER_API_KEY=sk-or-... python -m pytest -m live -v
```

## Guidelines

1. **No mock methods** — Use real implementations
2. **Mirror src/ structure** — Test files go in the matching subdir
3. **Shared fixtures** in `conftest.py`, shared helpers in `test_utils.py`
4. **Fixed seeds** for reproducibility (`np.random.seed(42)`)
5. **Zero warnings** target — avoid `Test*` naming for non-test helper classes
6. **Live API tests** gated with `@pytest.mark.live` and env-var checks
