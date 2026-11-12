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
`gate (Python 3.13)`) instead of `pytest (Python 3.x)`, so a branch-protection
rule that requires the old contexts needs updating when Actions billing returns;
the `gnn conformance (pinned)` job keeps its key (`gnn-conformance`) and display
name because that name is the status-check context.

## Environment

* `uv` **0.13.7** — `[tool.uv] required-version` in `pyproject.toml` enforces it.
* `uv.lock` is committed; `./run.sh setup` is
  `uv sync --locked --extra dev --extra llm`, so a run refuses a lock that
  disagrees with `pyproject.toml` instead of resolving something new.
* The `llm` extra is part of the gate environment, not an optional add-on: it is
  light (`aiohttp`, `openai`, `python-dotenv`, `requests`) and it keeps
  `tests/llm/test_openrouter_contract.py` and `tests/llm/test_llm_utils.py`
  running instead of module-skipping. The old CI expressed the same intent by
  installing `.[dev,llm]` on its 3.13 leg.
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
| `setup` | creates `.venv` from `uv.lock` with the dev and llm extras | both jobs |
| `lint` | `flake8` blocking selection: syntax errors, undefined names, broken f-strings over `src/`, `tests/`, `beyond_cerebrum/src/`, `scripts/` | `gate` |
| `types` | `mypy` on `src/core/gnn.py`, the annotated GNN loader surface | `gate` |
| `docs` | `docs/documentation-index.md` is current and every relative Markdown link resolves | `gate` |
| `test-fast` | quick pass over the logic core (see below) | local only |
| `test` | full suite with coverage and a JUnit report | `gate` |
| `gnn` | provisions `.gate/gnn`, calls its `gnn.importers` reader for pin/pointer/syntax validation, then runs CEREBRUM's model drift tests | `gnn-conformance` |
| `debt` | measures the non-blocking static-analysis debt | local only |
| `docs-index` | regenerates `docs/documentation-index.md` | — |
| `metrics` | regenerates `docs/_generated/gate-metrics.md` | — |

`./run.sh all` runs `lint`, `types`, `docs`, `test`, `gnn` in that order — the
same five steps the two CI jobs run between them.

The GNN–FEP reader migration is not yet landable: the current upstream pin
predates `gnn.importers`, so the `gnn` arm fails at that import. The
[GNN pointer](../.github/GNN.md#migration-blocker) records the exact dependency
and the bootstrap pin read that remains to be removed. Other arms are unchanged.

## Measured cost

Every number lives in one generated page:
[`_generated/gate-metrics.md`](_generated/gate-metrics.md). It carries the last
recorded run of each arm, the pytest counts and coverage from the JUnit and
coverage reports, the static-analysis debt counts, and the comparison against
the commands this gate replaced.

That comparison is generated too. The pre-gate receipts are stored as data in
[`../scripts/docgen/baseline.json`](../scripts/docgen/baseline.json) — a script
cannot re-measure a command the gate has replaced — and the page joins them to
the current arm timings, so the "now" column cannot drift from the artifacts.
Run the arms, then `./run.sh metrics`, then commit the page.

Two of those rows carry a caveat rather than a win. The full-suite receipt was
taken while another test run shared this host, so the before/after difference
there is host load, not the gate: no suite-speedup is claimed. The GNN receipt
describes work that only existed as inline workflow `bash`, so its "before" is a
shape, not a duration.

## Artifacts and generated documents

`./run.sh` writes to `.gate/` (gitignored): `timings.jsonl` (per-arm wall time
and exit), `env.json`, `junit.xml`, `coverage.json`, `debt.json`, and the pinned
GNN checkout. Two documents are generated from those artifacts and the tree:

| Document | Producer | Staleness check |
| --- | --- | --- |
| [`docs/_generated/gate-metrics.md`](_generated/gate-metrics.md) | `./run.sh metrics` | `./run.sh metrics --check` |
| [`docs/documentation-index.md`](documentation-index.md) | `./run.sh docs-index` | `./run.sh docs` |

Never type a number into those pages. Run the arms, then refresh the page; the
numbers in the metrics page are the ones the arms recorded. `metrics --check`
compares the page against the artifacts on disk, so it reports staleness after
any arm run that happened later than the last `./run.sh metrics`; refresh and
commit as the final step of a measurement.

The pre-gate receipts in `scripts/docgen/baseline.json` are the one input that
is not produced by an arm; the metrics page renders them next to the arm
timings, so the document still owns every number it shows.

## test-fast is a path selection, not a marker selection

`pytest.ini` declares `unit`, `integration`, `slow`, `visualization`,
`end_to_end` and `regression` markers, but the suite does not use them: no test
carries `slow`, `visualization` or `end_to_end`, so `pytest -m <marker>` selects
nothing, and most test files carry no marker at all. A marker-based "fast" arm
would therefore run essentially the whole suite. `test-fast` selects the logic
core by path instead (`tests/core`, `tests/cases`, `tests/transformations`,
`tests/utils`, and `beyond_cerebrum/src/tests`) and deselects the two slowest
core files, `tests/core/test_neural_network.py` and
`tests/core/test_active_inference_pomdp.py`. It is honest about what it covers: a
quick pre-flight, not a substitute for `./run.sh test`.

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

Arms made of more than one step (`docs`, `gnn`) refuse extra arguments instead
of guessing which step they belong to.
