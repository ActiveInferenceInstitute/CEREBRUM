# Scripts Context

## Purpose

Executable scripts for running tests and examples as thin orchestrators.

## Key Scripts

| Script | Purpose |
| ------ | ------- |
| `run_all_tests.py` | Main test runner |
| `run_all_examples.py` | Example orchestrator |
| `test_all_cases.py` | Case transformation tests |

## Usage

```python
# From Python
from src.scripts import run_tests, run_examples

run_tests()
run_examples()
```

```bash
# From command line (repository root)
python -m src.scripts.run_all_tests
python -m src.scripts.run_all_examples
```

## Thin Orchestrator Pattern

Scripts follow the "thin orchestrator" philosophy:

- Minimal logic in the script itself
- Delegate to module functions from `src/`
- Handle configuration and CLI parsing
- Log results and orchestrate execution flow
