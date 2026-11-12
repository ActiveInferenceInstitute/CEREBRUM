# Gate and CI run guide

`./run.sh` is the single gate entry point for this repository. It runs the arms
defined once in [`scripts/gate.py`](../scripts/gate.py), and the CI jobs in
[`.github/workflows/ci.yml`](../.github/workflows/ci.yml) do nothing but call
those same arms, one step per arm. There is no second copy of any command.

```bash
./run.sh              # list the arms
./run.sh all          # lint, types, docs, test, gnn
```

## Why the gate looks like this

Before this guide existed, the work was split between a four-leg pytest matrix
and a hand-written GNN job, and nothing else:

| Work | Before | Now |
| --- | --- | --- |
| Environment | each CI job created its own venv and installed `.[dev]` (4 test legs + 1 GNN leg) | one `uv.lock`, one `./run.sh setup` per job, 2 jobs |
| Test suite | 4 Python legs (3.10–3.13) ran the same suite | one leg on the repo's `.python-version` (3.13) |
| Lint / types / docs | no job at all | `lint`, `types`, `docs` arms, each also a CI step |
| GNN pin provisioning | inline `bash` in the workflow only | `scripts/gnn_checkout.py`, reached through `./run.sh gnn` |
| Measuring the gate | nothing recorded | `./run.sh` times every arm into `.gate/timings.jsonl` |

Dropping the 3.10–3.12 legs is a deliberate trade: they re-ran the same suite in
three more environments and CI is billing-locked for this organisation, so they
cost the most and reported nothing locally. `requires-python = ">=3.10"` still
describes the supported range; re-add a leg if version drift becomes a real
risk. The job that runs the suite is now named `gate` (display name
`gate (Python 3.13)`); the `gnn conformance (pinned)` job keeps its key
(`gnn-conformance`) and display name because that name is the status-check
context.

## Environment

* `uv` **0.13.7** — `[tool.uv] required-version` in `pyproject.toml` enforces it.
* `uv.lock` is committed; `./run.sh setup` is `uv sync --locked --extra dev`, so
  a run refuses a lock that disagrees with `pyproject.toml` instead of resolving
  something new.
* Python comes from `.python-version` (**3.13**) for the project environment;
  the GNN arm additionally provisions its own **3.12** interpreter for the
  pinned upstream parser.
* In CI, the setup step is the estate composite action
  `ActiveInferenceInstitute/.github/.github/actions/setup-python-uv@7159e67315330aac8e414aebae0ad1e999a41448`;
  locally, `uv` is whatever is on `PATH` (the lock and the required-version
  check keep the two honest).

## Arms

Commands live in `scripts/gate.py`; this table only says what each arm covers.

| Arm | What it checks | CI job |
| --- | --- | --- |
| `setup` | creates `.venv` from `uv.lock` with the dev extras | both jobs |
| `lint` | `flake8` blocking selection: syntax errors, undefined names, broken f-strings over `src/`, `tests/`, `beyond_cerebrum/src/`, `scripts/` | `gate` |
| `types` | `mypy` on `src/core/gnn.py`, the annotated GNN loader surface | `gate` |
| `docs` | `docs/documentation-index.md` is current and every relative Markdown link resolves | `gate` |
| `test-fast` | quick pass over the logic core (see below) | local only |
| `test` | full suite with coverage and a JUnit report | `gate` |
| `gnn` | pinned-upstream GNN conformance (provisions `.gate/gnn` at the pinned revision) | `gnn-conformance` |
| `debt` | measures the non-blocking static-analysis debt | local only |
| `docs-index` | regenerates `docs/documentation-index.md` | — |
| `metrics` | regenerates `docs/_generated/gate-metrics.md` | — |

`./run.sh all` runs `lint`, `types`, `docs`, `test`, `gnn` in that order — the
same five steps the two CI jobs run between them.

## Artifacts and generated documents

`./run.sh` writes to `.gate/` (gitignored): `timings.jsonl` (per-arm wall time
and exit), `env.json`, `junit.xml`, `coverage.json`, `debt.json`, and the pinned
GNN checkout. Two documents are generated from those artifacts and the tree:

| Document | Producer | Staleness check |
| --- | --- | --- |
| [`docs/_generated/gate-metrics.md`](_generated/gate-metrics.md) | `./run.sh metrics` | `./run.sh metrics --check` |
| [`docs/documentation-index.md`](documentation-index.md) | `./run.sh docs-index` | `./run.sh docs` |

Never type a number into those pages. Run the arms, then refresh the page; the
numbers in the metrics page are the ones the arms recorded.

## test-fast is a path selection, not a marker selection

`pytest.ini` declares `unit`, `integration`, `slow`, `visualization`,
`end_to_end` and `regression` markers, but the suite does not use them:
`pytest -m slow` selects **0** of the 1181 collected tests (likewise
`-m visualization` and `-m end_to_end`), and 60 of the 72 `test_*.py` files in
the collected trees carry no marker at all. A marker-based "fast" arm would
therefore run essentially the whole suite. `test-fast` selects the logic core by
path instead (`tests/core`, `tests/cases`, `tests/transformations`,
`tests/utils`, and `beyond_cerebrum/src/tests`), which is honest about what it
covers: it is a quick pre-flight, not a substitute for `./run.sh test`.

Populating the markers would let the arm become a marker selection; that is a
real change to the suite and is not part of this gate.

## Documentation check

There is no `mkdocs.yml`, no markdown-link-check config and no doc build in this
repository, so `./run.sh docs` uses two small scripts:

* [`scripts/docgen/doc_index.py`](../scripts/docgen/doc_index.py) — generates the
  flat index from the files that exist and fails on a stale page.
* [`scripts/check_links.py`](../scripts/check_links.py) — resolves every inline
  link, image and reference definition in tracked Markdown. It does not fetch
  external URLs. It reports leading-slash targets (site-absolute on GitHub, not
  repository links) as broken, and it skips three trees whose Markdown is input
  to, or output of, another tool: `paper/components/` (pandoc fragments assembled
  into `paper/output/`, where `Figure_N.png` is rendered from `Figure_N.md`),
  `paper/output/`, and `tests/output/`.

## Static-analysis debt

The gate blocks only what the tree currently passes; the rest is measured and
published in [`docs/_generated/gate-metrics.md`](_generated/gate-metrics.md)
rather than silently tolerated.

* `lint` blocks `E9,F63,F7,F82` only. The full `flake8` ruleset reports thousands
  of pre-existing style findings; reformatting the tree is a separate change.
* `types` covers `src/core/gnn.py`, which is annotated. `mypy src/` reports
  hundreds of pre-existing errors across the wider tree.
* `debt` records the current numbers for all four checks so a regression is
  visible even though it cannot be blocked yet.

## Reproducing a CI job locally

```bash
./run.sh setup && ./run.sh lint && ./run.sh types && ./run.sh docs && ./run.sh test   # the `gate` job
./run.sh setup && ./run.sh gnn                                                       # the `gnn-conformance` job
./run.sh all                                                                         # both, on one machine
./run.sh metrics                                                                     # refresh the measured numbers
```

## Manual invocation

`./run.sh <arm>` forwards extra arguments to the arm's command, so a single test
file or an extra pytest flag still works without bypassing the gate:

```bash
./run.sh test tests/core/test_model.py -v
./run.sh test-fast -x
```
