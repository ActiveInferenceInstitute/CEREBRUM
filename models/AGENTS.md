# Model Specification Context

## Purpose

This directory holds the Generalized Notation Notation (GNN) specifications for
the repository's generative models. Read `README.md` here first: it records the
declared structure, the loader and drift guard, the pinned upstream revision,
and the claim boundary.

## Rules

- The GNN file is the structural source of truth. Change the specification and
  the implementation together, never one side alone.
- `src/core/gnn.py` parses these files; `tests/core/test_gnn_spec.py` is the
  drift guard. Both must stay green when a model, parameter orientation, or
  case configuration changes.
- Keep the upstream pin in `.github/gnn-pin.json` in sync with `README.md`.
- Do not add calibrated or validated-ecological/behavioral claims here; these
  specifications describe implemented structure.
