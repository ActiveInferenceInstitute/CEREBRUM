# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Everything goes through one gate entry point; `./run.sh` lists the arms.
./run.sh setup         # create .venv from uv.lock (dev extras)
./run.sh lint          # blocking static checks (syntax / undefined names)
./run.sh types         # mypy on src/core/gnn.py
./run.sh docs          # docs index freshness + relative link check
./run.sh test-fast     # logic-core test selection
./run.sh test          # full suite with coverage + JUnit report
./run.sh gnn           # pinned-upstream GNN conformance (provisions .gate/gnn)
./run.sh all           # lint, types, docs, test, gnn

# Regenerate the generated docs after a run
./run.sh docs-index                       # docs/documentation-index.md
./run.sh debt                             # static-analysis debt counts
./run.sh metrics                          # docs/_generated/gate-metrics.md

# Debugging: a single test file
uv run python -m pytest tests/cases/test_case_manager.py -v

# GNN specification drift guard, against a sibling GNN checkout
GNN_CHECKOUT=../Generalized_Notation_Notation uv run python -m pytest tests/core/test_gnn_spec.py -v

# Fix formatting in files you touched (the tree is not black-clean; do not reformat
# unrelated files — see `./run.sh debt` for the current count)
black <file>; isort --profile black --line-length 100 <file>

# Run the comprehensive test suite across all case types
python src/scripts/test_all_cases.py

# Generate paper PDF (requires pandoc, XeLaTeX, Node.js with mermaid-cli)
python paper/assemble_paper.py
```

## Architecture

CEREBRUM is a cognitive modeling framework that maps **grammatical cases** (nominative, accusative, dative, genitive, instrumental, locative, ablative, vocative) to functional roles of Bayesian generative models. The core metaphor: a model's "case" determines its functional role in inference, just as grammatical case determines a noun's role in a sentence.

### Inheritance Hierarchy

```
src/core/model.py: Model (base) + Case (enum)
    └── src/core/active_inference.py: ActiveInferenceModel
            └── src/models/insect/base.py: InsectModel
            └── src/examples/animal_agent.py: AnimalAgent
src/models/base.py: Model (simplified re-export, re-exports Case from core)
```

**Note:** There are two `Model` classes — `src/core/model.py` (full, with precision weights and case history) and `src/models/base.py` (simplified). Both share the same `Case` enum via re-export. Prefer `src.core.model` for new model implementations.

### Key Modules

- **`src/core/model.py`** — `Case` enum (8 cases: NOM/ACC/GEN/DAT/INS/LOC/ABL/VOC) and base `Model` class with case-switching logic. Changing `model.case` triggers `_apply_case_transformation()`.
- **`src/core/active_inference.py`** — `ActiveInferenceModel` extending `Model` with variational free energy minimization, precision-weighted message passing, and belief updating via Bayesian inference.
- **`src/core/gnn.py`** — loads the Generalized Notation Notation specification in `models/` and reports conformance against the model parameters and case configurations; see `models/README.md`.
- **`src/cases/`** — One file per case (e.g., `nominative.py`, `accusative.py`). `case_manager.py` orchestrates transitions. Cases define how the model behaves when in that role.
- **`src/transformations/case_transformations.py`** — `transform_case(model, target_case)` and `revert_case(model)` functions. `animal_transformations.py` has animal-specific variants.
- **`src/models/insect/`** — Domain-specific insect cognitive models (`base.py`, `behaviors.py`, `neural_structures.py`, `species.py`) built on `ActiveInferenceModel`. Uses `BehavioralState` enum and structured `SensoryInput`/`Action` dataclasses.
- **`src/lexicon/`** — LEXICON subsystem: NLP pipeline for processing text into knowledge graphs. Has its own engine (`core/engine.py`), NLP components, graph assembler, and visualization. Requires OpenRouter API (`.env` with API keys).
- **`src/llm/`** — LLM integrations: OpenRouter client (`OpenRouter/openrouter.py`) and Ollama client. Config loaded from `.env` at repo root.
- **`src/visualization/`** — Matplotlib-based visualizers for case comparisons, model states, and animations.
- **`beyond_cerebrum/`** — Extended formalisms (category theory, type theory) that build on CEREBRUM. Has its own `src/` and `tests/` layout mirroring the main repo.

### Test Layout

Tests moved from `src/tests/` to top-level `tests/`. Pytest config (`pytest.ini`) points to both `tests/` and `beyond_cerebrum/src/tests/`. Several test files are ignored by default (POMDP test cases, some linear regression tests) due to unimplemented or optional-dependency modules.

### Case System Usage Pattern

```python
from src.core.model import Model, Case
from src.transformations import transform_case, revert_case

model = Model(name="my_model")
transform_case(model, Case.ACCUSATIVE)  # model becomes object-under-evaluation
revert_case(model)                       # returns to prior case

# Or via CaseManager
from src.cases import CaseManager
manager = CaseManager(model)
manager.transition(Case.DATIVE)
```

### Environment Variables

The LLM and LEXICON subsystems read from `.env` at the repo root. Required keys for those features: `OPENROUTER_API_KEY` (for OpenRouter), Ollama runs locally on default port.
