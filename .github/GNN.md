# GNN conformance

`gnn conformance (pinned)` is the status-check name of the `gnn-conformance`
job in `.github/workflows/ci.yml`. Locally the same work is the `gnn` arm of the
gate: `./run.sh gnn` (see [../docs/RUN_GUIDE.md](../docs/RUN_GUIDE.md)).

## Pin

| Field | Value |
| --- | --- |
| Pin file | `.github/gnn-pin.json` |
| Schema | `gnn-importer-pin/v1` |
| Upstream | `ActiveInferenceInstitute/Generalized_Notation_Notation` |
| Tag | `v5.0.3` |
| Revision | `8f293d9ba810e6eda5c09842d9c7ed9ffa5f92c1` |
| Recorded | `2026-11-12` |
| Specification directory | `models` |
| Specification files | `models/case_conditioned_active_inference.gnn.md` |

`.github/gnn-pin.json` is the only place the revision is written down; the job,
`scripts/gnn_checkout.py`, `models/README.md` and the test all read it from
there.

## What the job parses

* `scripts/gnn_checkout.py` materialises the pinned revision into `.gate/gnn`:
  `git init`, `git fetch --depth 1 origin <revision> refs/tags/<tag>`,
  `git checkout --detach <revision>`, then a private `uv venv --python 3.12`
  with the parser's runtime dependencies (`numpy`, `defusedxml`, `psutil`).
  A revision that no longer resolves upstream fails the arm instead of skipping.
* `tests/core/test_gnn_spec.py` reads the specification named by
  `gnn-pin.json` (`models/case_conditioned_active_inference.gnn.md`) through
  `src/core/gnn.py`, which parses it without importing the upstream package. It
  checks the declared factors, modality, controls, matrix values and axis
  orders against `InsectModel` / `ActiveInferenceModel`, and that each case
  transformation rescales exactly the matrices the specification declares for
  that case's precision focus.
* With `GNN_CHECKOUT` set — the arm always sets it, and so does CI — the test
  also exports the pinned revision (`git archive <revision> src/gnn`) into a
  temporary tree and runs the pinned upstream `gnn.cli validate --strict`
  against this repository's specification with that checkout's interpreter. An
  in-progress sibling checkout cannot contaminate the result because the export
  is taken from the pinned commit only.

The check covers the one selected specification. It is not a scan of every
`*.gnn.md` in the tree, and it says nothing about behavioural or biological
validity of the declared model.

## Environment

Both CI jobs set up Python and uv with the estate composite action, pinned by
commit: `ActiveInferenceInstitute/.github/.github/actions/setup-python-uv@7159e67315330aac8e414aebae0ad1e999a41448`.
It installs a pinned `uv` (`0.13.7`, which `[tool.uv] required-version` in
`pyproject.toml` also enforces) and a CPython interpreter; the project
environment itself comes from `./run.sh setup` (`uv sync --locked --extra dev`).
