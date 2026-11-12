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

`.github/gnn-pin.json` owns the upstream identity. While the pinned release
predates `gnn.importers`, `scripts/gnn_checkout.py` is the local pin reader and
checks pin/pointer drift. The runtime tests call that same reader and retain
model agreement, pinned-parser acceptance and exported-runtime identity checks.

## What the job parses

* `scripts/gnn_checkout.py` materialises the pinned revision into `.gate/gnn`:
  `git init`, `git fetch --depth 1 origin <revision> refs/tags/<tag>`,
  `git checkout --detach <revision>`, then a private `uv venv --python 3.12`
  with the parser's runtime dependencies (`numpy`, `defusedxml`, `psutil`).
  A revision that no longer resolves upstream fails the arm instead of skipping.
* Before provisioning or reusing a checkout, the script checks the pin's schema,
  revision and declared specifications, and requires this pointer to name the
  pin path, revision, workflow and job. The same drift check runs without network
  or provisioning via:

  ```bash
  .venv/bin/python scripts/gnn_checkout.py --check
  ```

* `tests/core/test_gnn_spec.py` loads
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

## Migration blocker

The pinned release does not contain `src/gnn/importers.py`. The updated
[GNN–FEP prerequisite](https://github.com/ActiveInferenceInstitute/Generalized_Notation_Notation/blob/d31c1768114098e91033ad2ad5a383e188c3cd67/docs/workflows/gnn-fep.md#prerequisite-the-reader-ships-in-a-revision-not-in-a-version-name)
requires members in this state to retain their local reader and add pin/pointer
drift checks, not call the unavailable shared module or vendor it. This gate
implements that interim path; it does not claim the full shared-reader cutover.

Cutover waits for an upstream release containing the reader. The same reviewed
change must advance this pin, remove the local reader, call `gnn.importers`
from the new pinned runtime, and record the old and new revisions. The pin and
schema are unchanged here; no unpinned bootstrap reader or parser overlay is used.

The check covers the one selected specification. It is not a scan of every
`*.gnn.md` in the tree, and it says nothing about behavioural or biological
validity of the declared model.

## Environment

Both CI jobs set up Python and uv with the estate composite action, pinned by
commit: `ActiveInferenceInstitute/.github/.github/actions/setup-python-uv@7159e67315330aac8e414aebae0ad1e999a41448`.
It installs a pinned `uv` (`0.13.7`, which `[tool.uv] required-version` in
`pyproject.toml` also enforces) and a CPython interpreter; the project
environment itself comes from `./run.sh setup` (`uv sync --locked --extra dev`).
