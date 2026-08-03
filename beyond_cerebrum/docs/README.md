# FORMICA Documentation

This directory contains the detailed documentation for the FORMICA framework, mirroring the structure of the `src/` directory.

See the main [FORMICA Specification](../beyond_spec.md) for the overall vision and design.

## Modules

- [Formalisms](./formalisms/README.md): Core linguistic representations (types, structures, categories). Theory notes live in [`formalisms/theory/`](formalisms/theory/README.md).

The following modules have source code but do not yet have dedicated
documentation pages (see their source packages instead):

- **Operations** (`src/operations/`): Computational calculus (`calculus.py`, `transformations.py`, `inference.py`)
- **Analysis** (`src/analysis/`): Interpretability and monitoring tools (`interpretability_hooks.py`, `lexical_forensics.py`)
- **Visualization** (`src/visualization/`): Visualization utilities (`visualize.py`)
- **Backends** (`src/backends/`): Interfaces to computational models (placeholder)
- **Interfaces** (`src/interfaces/`): Input/Output handling (placeholder)
- **Utils** (`src/utils/`): Utility functions (placeholder)
