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

`.github/gnn-pin.json` owns the upstream identity. The gate delegates pin,
pointer and specification validation to `gnn.importers`; CEREBRUM's tests
retain only the model/specification agreement checks.

## What the job parses

* `scripts/gnn_checkout.py` materialises the pinned revision into `.gate/gnn`:
  `git init`, `git fetch --depth 1 origin <revision> refs/tags/<tag>`,
  `git checkout --detach <revision>`, then a private `uv venv --python 3.12`
  with the parser's runtime dependencies (`numpy`, `defusedxml`, `psutil`).
  A revision that no longer resolves upstream fails the arm instead of skipping.
* The gate invokes the reader from that checkout's interpreter and `src/`:

  ```bash
  PYTHONPATH=.gate/gnn/src .gate/gnn/.venv/bin/python -m gnn.importers check \
      --repo . --require-pointer --workflow .github/workflows/ci.yml \
      --job gnn-conformance
  ```

* `tests/core/test_gnn_spec.py` loads
  `models/case_conditioned_active_inference.gnn.md` through `src/core/gnn.py`.
  It checks the declared factors, modality, controls, matrix values and axis
  orders against `InsectModel` / `ActiveInferenceModel`, and that each case
  transformation rescales exactly the matrices declared for its precision focus.

## Migration blocker

The pinned revision does not contain `src/gnn/importers.py`. The reader call
therefore fails instead of substituting an unpinned implementation. The
[GNN–FEP contract](https://github.com/ActiveInferenceInstitute/Generalized_Notation_Notation/blob/5efd8b2922c678e99e754c9f1ec2a5bd63b62cc2/docs/workflows/gnn-fep.md)
requires the reader and parser from the same pinned runtime.

This migration cannot land until a reviewed upstream revision containing the
reader is adopted, and clean-checkout provisioning can call its `read_pin`.
`scripts/gnn_checkout.py` still has the bootstrap JSON read; it is not a
completed migration. No schema or pin identity is changed here.

The check covers the one selected specification. It is not a scan of every
`*.gnn.md` in the tree, and it says nothing about behavioural or biological
validity of the declared model.

## Environment

Both CI jobs set up Python and uv with the estate composite action, pinned by
commit: `ActiveInferenceInstitute/.github/.github/actions/setup-python-uv@7159e67315330aac8e414aebae0ad1e999a41448`.
It installs a pinned `uv` (`0.13.7`, which `[tool.uv] required-version` in
`pyproject.toml` also enforces) and a CPython interpreter; the project
environment itself comes from `./run.sh setup` (`uv sync --locked --extra dev`).
