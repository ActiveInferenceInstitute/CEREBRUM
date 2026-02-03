# Examples Context

## Purpose

Runnable example scripts demonstrating CEREBRUM functionality.

## Running Examples

```bash
# From repository root
python -m src.examples.insect_simulation_example
python -m src.examples.animal_agent
```

Or using the scripts module:

```bash
python -c "from src.scripts import run_examples; run_examples()"
```

## Key Examples

| Example | Purpose |
| ------- | ------- |
| `insect_simulation_example.py` | Demonstrates insect model with Active Inference |
| `animal_agent.py` | Animal agent with case transformations |

## Usage as Reference

These files are excellent references for:

- How to instantiate Agents and Environments
- Case transformation patterns
- Visualization integration
- Simulation loop structure

## Thin Orchestrator Pattern

Examples should follow the "thin orchestrator" pattern:

- Minimal logic in the script itself
- Delegate to module functions from `src/`
- Focus on demonstration and configuration
