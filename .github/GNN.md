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
| Tag | `v5.1.0` |
| Revision | `75c834bd8519dc7b5adc4d24f16bf393cfd1ecdf` |
| Recorded | `2026-11-12` |
| Specification directory | `models` |
| Specification files | `models/case_conditioned_active_inference.gnn.md` |

`.github/gnn-pin.json` owns the upstream identity. The pin is read and judged by
`gnn.importers`, the estate's one reader for a pin, its declared specifications
and this pointer; this repository carries no copy of that logic.

## What the job parses

* `scripts/gnn_checkout.py` materialises the pinned revision into `.gate/gnn`:
  `git init`, `git fetch --depth 1 origin <revision> refs/tags/<tag>`,
  `git checkout --detach <revision>`, then a private `uv venv --python 3.12`
  with the parser's runtime dependencies (`numpy`, `defusedxml`, `psutil`,
  `pyyaml`). A revision that no longer resolves upstream fails the arm instead
  of skipping. The script takes only those two `git fetch` coordinates from the
  pin; whether they are the right ones is the reader's verdict, not its own.
* That same environment then runs the shipped reader against this repository:

  ```bash
  PYTHONPATH=.gate/gnn/src .gate/gnn/.venv/bin/python -m gnn.importers check \
      --repo . --require-pointer \
      --workflow .github/workflows/ci.yml --job gnn-conformance
  ```

  The pinned tree supplies `gnn`; the private venv supplies its dependencies, so
  the reader, the parser and the checkout are one revision. The reader reports
  the pin's schema, revision shape, tag and declared specifications, parses the
  declared specification with the pinned parser, and requires this pointer to
  name the pin path, the pinned revision, the workflow filename and the job key.
* `tests/core/test_gnn_spec.py` reports that same reader verdict, then loads
  `models/case_conditioned_active_inference.gnn.md` through `src/core/gnn.py`.
  It checks the declared factors, modality, controls, matrix values and axis
  orders against `InsectModel` / `ActiveInferenceModel`, and that each case
  transformation rescales exactly the matrices declared for its precision focus.
* The parser-acceptance and runtime-identity tests use a clean
  `git archive <revision> src/gnn pyproject.toml` export and that checkout's
  interpreter. They assert strict parser acceptance, the imported module's
  location inside the export, and its declared version. The tag-resolution test
  independently checks that the declared tag dereferences to the pinned commit.
  An explicitly selected `GNN_CHECKOUT` that cannot supply this runtime fails
  rather than skipping.

## Adopted: one reader, no local copy

GNN `v5.1.0` at `75c834bd8519dc7b5adc4d24f16bf393cfd1ecdf` is the first pinned
release that contains `src/gnn/importers.py`. The previous pin, `v5.0.3` at
`8f293d9ba810e6eda5c09842d9c7ed9ffa5f92c1`, predates the module, so the
intervening state kept a local reader — `load_pin` in
`scripts/gnn_checkout.py`, with the pointer regressions that tested its own pin
shape — and could not call `python -m gnn.importers`.

This change advances the pin to `v5.1.0`, deletes that reader and the tests for
its pin shape, and runs the shipped reader from the environment the arm already
builds. `validate`, `pin-commit-shape`, `pin-tag-mismatch`, `spec-missing`,
`spec-invalid` and the `pointer-*` codes are the reader's findings now; the
copy carried no unique coverage. No copy of `importers.py` is vendored and no
newer reader is overlaid on an older parser.

The check covers the one declared specification. It is not a scan of every
`*.gnn.md` in the tree, and it says nothing about behavioural or biological
validity of the declared model.

## Environment

Both CI jobs set up Python and uv with the estate composite action, pinned by
commit: `ActiveInferenceInstitute/.github/.github/actions/setup-python-uv@7159e67315330aac8e414aebae0ad1e999a41448`.
It installs a pinned `uv` (`0.13.7`, which `[tool.uv] required-version` in
`pyproject.toml` also enforces) and a CPython interpreter; the project
environment itself comes from `./run.sh setup` (`uv sync --locked --extra dev`).
