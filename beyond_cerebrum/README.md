# Beyond CEREBRUM: FORMICA Conceptualization

This directory contains conceptual work and initial planning for FORMICA (**FO**rmal **R**epresentation and **M**odeling **I**ntegrating **C**omprehensive **A**spects), a proposed framework extending beyond the scope of CEREBRUM.

## Contents

*   `beyond_spec.md`: The primary specification document outlining the vision, goals, architecture, challenges, and potential roadmap for FORMICA.
*   `src/`: An initial, partially implemented source tree for the FORMICA framework. Current state:
    *   `formalisms/`: Linguistic structure representations (`categories.py`, `structures.py`, `types.py`, plus theory notes under `formalisms/theory/`).
    *   `operations/`: Computational actions on linguistic structures (`calculus.py`, `inference.py`, `transformations.py`).
    *   `analysis/`: Tools for interpretability and lexical environment forensics (`interpretability_hooks.py`, `lexical_forensics.py`).
    *   `visualization/`: Visualization utilities (`visualize.py`).
    *   `backends/`, `interfaces/`, `utils/`: Currently placeholder packages (module initialization only).
    *   `tests/`: Initial tests for `formalisms` and `operations`.
*   `docs/`: Documentation for the FORMICA framework (see [docs/README.md](docs/README.md)).

## Status

**Conceptual / exploratory.** The specification is a design proposal, and the
source tree is a partial scaffold. This directory is not part of the core
CEREBRUM framework and is not production-ready.

## Purpose

This directory serves as a sandbox for exploring the theoretical underpinnings and high-level design of a comprehensive, model-agnostic computational framework for language, aiming to formalize all aspects of linguistic intelligence.
