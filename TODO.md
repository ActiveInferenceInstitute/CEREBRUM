# CEREBRUM — Project Backlog

> **Status**: Active
> **Owner**: Daniel Ari Friedman
> **Last reviewed**: 2026-11-12

This is the authoritative project backlog. It records completed work and the
items that are still open. Every completed line names the commit, PR or file that
proves it; every open item names what blocks it and who can unblock it.
**Open — 2026-11-12** at the end of this file is the current open list; the dated
pass sections above it are history and are kept for the evidence trail.

---

## Completed / Closed

Implemented during the 2026-08-01 review/remediation passes. Full `pytest tests/`
suite passes: **1178 passed, 3 skipped, 0 failed** (verified EXIT=0 on a clean
run; the baseline was 1 failed / 1144 passed / 3 skipped). Targeted module
suites (core, cases, transformations, utils/data_generator, lexicon, visualization)
all green (442 passed in the combined post-isort check).

### Gate, CI and documentation (2026-11-12)
- [x] **One gate entry point** — `./run.sh` dispatches the arms defined in
      `scripts/gate.py` (`setup`, `lint`, `types`, `docs`, `test-fast`, `test`,
      `gnn`, `debt`, `metrics`, `docs-index`); every arm is timed into
      `.gate/timings.jsonl`. See [docs/RUN_GUIDE.md](docs/RUN_GUIDE.md).
- [x] **CI calls the arms** — two jobs (`gate`, `gnn-conformance`) instead of a
      four-leg pytest matrix plus a hand-written GNN job; one `uv.lock`, one
      `uv sync --locked --extra dev --extra llm` per job, Python and uv from the
      pinned estate composite action.
- [x] **GNN provisioning scripted** — `scripts/gnn_checkout.py` materialises the
      revision in `.github/gnn-pin.json` (`gnn-importer-pin/v1`, `v5.1.0`,
      `75c834bd8519dc7b5adc4d24f16bf393cfd1ecdf`) into `.gate/gnn`, so the
      workflow and a local run provision it the same way.
- [x] **Shared GNN reader adopted** — the pin advanced from `v5.0.3`
      (`8f293d9ba810e6eda5c09842d9c7ed9ffa5f92c1`, which predates the reader) to
      `v5.1.0`, which ships `src/gnn/importers.py`. The `gnn` arm now runs
      `python -m gnn.importers check --repo . --require-pointer --workflow
      .github/workflows/ci.yml --job gnn-conformance` from the interpreter it
      provisions, and the interim local reader (`load_pin`) and its pin-shape
      tests are deleted. No copy of the reader is vendored; see
      [.github/GNN.md](.github/GNN.md) and the
      [GNN–FEP contract](https://github.com/ActiveInferenceInstitute/Generalized_Notation_Notation/blob/main/docs/workflows/gnn-fep.md).
- [x] **Generated documentation** — `docs/documentation-index.md` (flat index of
      the `docs/` tree) and `docs/_generated/gate-metrics.md` (per-arm wall
      time, test counts, coverage, static-analysis debt) are produced by
      scripts and checked by the `docs`/`metrics` arms; no measured number is
      typed by hand. The `metrics` arm tolerates a fresh clone with no `.gate/`
      artifacts (`e8ffc52`, #18), and the page is the only place measured
      numbers live (`bef71b4`, #19).
- [x] **Link check** — `scripts/check_links.py` resolves every relative Markdown
      link (no network). It found and we fixed an unresolvable
      `/Supplement_C_3_...` target and a prose line that Markdown parsed as a
      link definition.
- [x] **Manuscript assembly reports failure** — `paper/assemble_paper.py` now
      runs as `main() -> int` and exits nonzero when Mermaid rendering,
      Markdown assembly, HTML conversion or PDF conversion fails, while still
      attempting the remaining independent steps; a failed assembly skips both
      conversions. `paper/README.md` documents the contract and
      `tests/test_paper_assembly.py` covers it. Also aligns the `.aii` sidecar
      `tasks.test` command with `./run.sh test`. (`6837324`, #21.)

### Mathematics / Core correctness
- [x] **free_energy() KL-divergence corrected** (`src/core/active_inference.py`).
      The complexity term used an inverted trace and an inverted log-det ratio;
      rewrote as `0.5*(tr(Pprior @ inv(Ppost)) + (μp-μq)'Pprior(μp-μq) - n + log(det(Ppost)/det(Pprior)))`,
      factored into a numerically-guarded `_kl_divergence()` helper (handles
      singular precision matrices). Added `TestKLCorrectness` asserting the value
      matches an analytic KL reference and is non-negative.
- [x] **InsectModel dimension mismatch fixed** — `n_states`/`n_observations` now
      read from `parameters` when `prior_means` absent, so `free_energy()`,
      `_update_genitive()`, and VOCATIVE status no longer crash on the demo model.
      Added `TestDefaultModelRobustness`.
- [x] **Default-model robustness** — `get_optimal_action`, `predict_next_state`,
      `predict_observation` no longer `KeyError` on bare `ActiveInferenceModel()`.
- [x] **update_posterior no longer silently keeps the prior on all-zero posterior** —
      now returns a clear `error` status instead of reporting success with a stale belief.

### Architecture
- [x] **Dual-Model base classes unified** — `src/models/base.py` `Model` is now a
      thin, back-compatible subclass of the full `src.core.model.Model`. 
      `LinearRegressionModel` now has `connect()`, `_prior_case`, `get_precision()`,
      `_case_history`, and real case transformations (`create_case_relationship` /
      `revert_case` no longer crash). Added regression tests in
      `tests/transformations/test_case_transformations.py`.
- [x] **`src/scripts` package made importable** — wrong import path, missing `main()`
      on both runners, and the hard dependency on the (pytest-ignored) linear-regression
      backend are fixed; exports are lazy. `import src.scripts`, `run_tests`,
      `run_examples` work.
- [x] **CaseManager.calculate_free_energy** now prefers the model's real
      `free_energy()` when implemented (H10 dead-code fixed); added regression test.

### Lexicon subsystem (src/lexicon/ + tests/lexicon/)
- [x] **Determinism** — `structured_case_determiner.py` `random.choice` fallback
      replaced with stable MD5-derived case assignment.
- [x] **Secrets** — `LexiconConfig.save()` no longer writes the plaintext
      `openrouter_api_key` to disk.
- [x] **Coreference data corruption** — `coreference_resolver.py` re-segments via
      safe mapping instead of character-offset slicing.
- [x] **ASR audit** — external-whisper fallback reads the correct output file;
      subprocess has a timeout.
- [x] **File watcher** — adds retry + thread-safety around `known_files`.
- [x] **Entity dedup / stable node IDs** — wired into graph assembly.
- [x] **Batch/session stem collisions** — unique per-file directories.
- [x] **Entity-linker string confidence** — coerced to float (no silent drop).
- [x] **process_audio `metadata=None`** — guard moved before first use.
- [x] Minor: remove stray `print()` leaks; except-tuple cleanup; paraphrase cache
      empty-list handling.

### Visualization subsystem (src/visualization/ + src/utils/animation.py)
- [x] **Fabricated-data removal** — `report_generator.py` no longer injects
      `np.random` metrics; computes from real event data and uses `.get()` defaults.
- [x] **Command injection fixed** — `src/utils/animation.py` ffmpeg conversion uses
      `subprocess.run([list])` (no `os.system`/shell).
- [x] **Model-mutation removed** — `neural_visualizer.track_learning_dynamics` no
      longer calls `structure.update_weights`; `plot_free_energy_landscape` operates
      on a copy and restores state in `try/finally` (also fixes the 1D IndexError).
- [x] **simulation_logger export/file divergence** — unbounded `_all_events` history
      kept for export fidelity; log files reset per run (no interleaving).
- [x] **comprehensive_visualizer** — documented as illustrative/synthetic and seeded
      deterministically (reproducible demo output); real metrics preserved.
- [x] **behavior_visualizer** empty-panel / orphaned-figure items addressed.
- [x] Minor: figure leaks, KeyError-prone event access, json-safe saves.

### Utils / data
- [x] **`classification_data` silent truncation + class-separation bug fixed**
      (`src/utils/data_generator.py`) — exactly `n_samples` rows, each class on its
      own feature dimension, `ValueError` on degenerate input; tests updated.
- [x] **`split_data`** validates `test_size ∈ [0,1]`.

### Scripts / setup
- [x] **setup_cerebrum.py** uses `uv` and validates `pyproject.toml` (dropped
      nonexistent `requirements*.txt`).
- [x] **`_backward_with_metrics`** AttributeError fixed (uses `self.activation`);
      tests added.

### Docs / manuscript
- [x] README version + BibTeX → 1.5.0; test-count claims in AGENTS.md files updated.
- [x] Paper title page → "Version 1.5.0 (2025-04-16)".
- [x] TODO.md created (this file) as the authoritative backlog.

### GNN specification, verification gate (2026-11-01)
- [x] **Generalized Notation Notation specification for the discrete model** —
      `models/case_conditioned_active_inference.gnn.md` declares the hidden-state
      factor, observation modality, identity likelihood, three controls, identity
      transition, uniform prior, the runtime axis orders
      (`A[state, observation]`, `B[previous_state, action, next_state]`), and the
      eight-case precision-focus contract. `src/core/gnn.py` parses it without
      upstream dependencies; `tests/core/test_gnn_spec.py` compares it with the
      `InsectModel` parameters and the `ActiveInferenceModel` case
      configurations, checks the stored orientations with asymmetric matrices,
      verifies each case transformation rescales exactly the declared matrices,
      and re-validates the file with the pinned upstream parser (`--strict`) from
      a clean export of the pinned revision. Pin: GNN v5.1.0,
      `75c834bd8519dc7b5adc4d24f16bf393cfd1ecdf`, recorded in
      `.github/gnn-pin.json`.
- [x] **Documented test gate collects cleanly on a `.[dev]` install** — seven test
      modules (`tests/lexicon/*`, `tests/llm/test_llm_utils.py`,
      `tests/llm/test_openrouter_contract.py`, `tests/llm/test_openrouter_structure.py`)
      import optional extras (`python-dotenv`, `requests`, `openai`) that the
      documented `uv pip install -e ".[dev]"` command does not install, so the CI
      job failed with six collection errors on every observed run between
      2026-08-03 and 2026-09-01; with collection repaired, the structure tests
      then failed individually because they import inside test functions. Each
      suite now skips with an explicit reason naming the missing dependency
      instead of erroring, `pytest --collect-only` collects the full suite, and
      the suites run normally when the extras are installed. The Python 3.13 CI
      leg now installs the light `llm` extra, so the OpenRouter contract tests run
      there instead of skipping; `live`-marked tests stay out of the default run
      (`-m "not live"` in `pytest.ini`).
- [x] **Metadata and signposting** — the package classifiers list Python 3.13,
      which the CI matrix already tests, and the README points to related Active
      Inference Institute repositories that already declare models in GNN.

---

## Major — Scoped (closed)

The one Major-class item deferred from the 2026-08-01 pass is closed; the entry
is kept with its closure evidence.

### 1. Lexicon component-architecture full unification — closed 2026-08-18
- **Files**: `src/lexicon/core/engine.py` getters `_get_nlp_preprocessor` /
  `_get_case_tagger` / `_get_paraphrase_generator`; the
  `NLPPreprocessor`/`CaseTagger` classes
- **Why it mattered**: The **data-integrity portion was already done and
  verified** — `_get_entity_deduplicator` + `_get_graph_assembler` are wired
  into `_build_knowledge_graph`, producing deduplicated, stable CID-based
  node/claim IDs (verified: 3 input entities incl. a duplicate → 2 unique
  nodes, stable IDs). The remaining gap was that the shipped `process_text`
  path kept its own inline LLM logic while the spaCy-based `NLPPreprocessor` /
  `CaseTagger` component classes sat in a separate (largely unused) pipeline.
- **Closed**: `process_text` now runs the unified pipeline — `NLPPreprocessor`
  segments the text and `CaseTagger` applies the 8-case declension, surfaced as
  `result["segments"]` / `result["cased_segments"]` in
  `src/lexicon/core/engine.py`, with both stages degrading gracefully (warning
  log, never abort). Verified live end-to-end against a real OpenRouter key
  (see "Second pass — 2026-08-18" below) and guarded by
  `src/lexicon/tests/test_integration_live.py` (`live` marker, skipped without
  `OPENROUTER_API_KEY`).

---

## Completed / Closed (second pass — 2026-08-01, deferred items closed)

These deferred items were implemented and verified after the first push.

- [x] **`revert_case` multi-step history (H3)** — `Model` now maintains a real
      `_case_stack`; every transition pushes the outgoing case, and `Model.revert()`
      pops it so repeated calls walk back through the full history instead of
      toggling. `revert_case()` prefers `model.revert()` and falls back to
      `_prior_case`. Added `test_revert_case_multi_step_history`.
- [x] **Global `np.random.seed()` mutation in data generators (MED-3)** — all
      `DataGenerator` methods now use a local `np.random.RandomState(seed)` via
      `DataGenerator._rng()` (byte-identical output, non-global). Verified global
      RNG state is unchanged after generation; 30 tests pass.
- [x] **Optional-dependency fail-open/closed consistency (visualization)** —
      `seaborn` import in `comprehensive_visualizer` wrapped in try/except and
      usage guarded (style selection version-fragile-proofed); `nx.DiGraph()` uses
      in `neural_visualizer` (2) and `case_visualization.plot_model_ecosystem` (1)
      are now guarded (`nx is None` → placeholder figure / clear ImportError);
      `src/visualization/__init__.py` now distinguishes `ModuleNotFoundError`
      (graceful degrade, logged) from genuine internal `ImportError` (surfaced).


---

## Docs review pass — 2026-08-02

Mega-deep documentation review (see `REVIEW_LOG_2026-08-02.md` for the full
findings). Test baseline verified on this date: **1242 collected, 1239 passed,
3 skipped, 13 warnings**.

### Minor (✓ completed)

- [x] **Duplicate BibTeX `url` key in README** — removed the DOI `url` line (kept GitHub `url` + `doi` field). (`README.md`, `0e42932`)
- [x] **Overlapping Zenodo badges** — removed the redundant repo badge; kept the DOI badge. (`README.md`, `0e42932`)
- [x] **`.mermaid` embeds in README** — GitHub cannot render them; converted to source links. (`README.md`, `0e42932`)
- [x] **Broken `../CEREBRUM.md` link** in the examples index → core spec. (`docs/examples/Examples_README.md`, `f1f08f9`)
- [x] **Missing `Case` import** in insects quick-start. (`docs/insects/README.md`, `f1f08f9`)
- [x] **Nonexistent `examples.py`** listed in `src/cases/README.md`. (`7b1fb51`)
- [x] **"9 modules" → 8** in `src/visualization/README.md`. (`7b1fb51`)
- [x] **`generated/` description** in `docs/diagrams/README.md` (PNG/SVG → `.mermaid` copies).
- [x] **Missing language index entries** — Lean/Nim/OCaml added to `docs/languages/README.md`; 8 languages added to `computer/README.md`. (`f1f08f9`)

### Medium (✓ completed)

- [x] **Stale test counts (5 files)** — 405/253/1152/1149 → measured 1242/1239/3; "0 warnings" → 13 warnings. (`0e42932`)
- [x] **License mismatch** — README, `docs/README.md`, `pyproject.toml` → CC BY 4.0 to match `LICENSE`. (`0e42932`)
- [x] **Three overlapping tests READMEs** — consolidated into `README.md` (index) + `README_TESTING.md` (methodology + dependencies + troubleshooting); `Testing_README.md` deleted; commands unified to `uv run python -m pytest`. (`0e42932`)
- [x] **`getting_started.md` fictional API** — rewritten against the verified real API (`src.Model`, `Case` enum, `transform_case()`, `register_model()`); example executed. (`f19d991`)
- [x] **`how_it_works.md` overclaims** — Precision Allocator / Message Bus marked spec-only; JS/Rust marked guidelines. (`f19d991`)
- [x] **`model_examples.md` fictional imports** — illustrative-style banner + real API imports. (`f19d991`)
- [x] **Examples index missing 06–08** — added POMDP / Linear Regression / Neural Network rows. (`f1f08f9`)
- [x] **Speculative-design index** — full 58-file index added; 0-byte `case_evolution_today.md` deleted; 4 broken image refs → honest diagram notes. (`f1f08f9`)
- [x] **Insects status contradiction** — banners on `assessment-summary.md` / `implementation-roadmap.md` reconciling with the completed implementation. (`f1f08f9`)
- [x] **`src/llm` drift (large)** — README/AGENTS/docs rewritten to implemented reality; `__init__.py` vendored docstring ("Corym Library") replaced; planned components marked as such. (`8e2761f`)
- [x] **Contributing guides** — 8 dead doc links, nonexistent `examples/python|js|rust`, unverifiable Discord invites/emails → real targets + GitHub channels. (`7b1fb51`)
- [x] **`beyond_cerebrum` READMEs** — "placeholder" claim corrected to actual modules; dead module links fixed. (`7b1fb51`)
- [x] **Language index clickability** — 90+ backtick filenames → real links (`computer/` prefix); stale gap/roadmap lists fixed. (`f1f08f9`)

### Major (✓ completed / ○ deferred)

- [x] **Paper figure path convention** — 17 placement tags in 5 components + `assembled_paper.md` + HTML normalized to bare `Figure_N.png` (matching `assemble_paper.py`); README clarified. (`60de1e1`)
- [x] **Private `~/.claude/` artifact removed** from the public repo; `~/` + `.desloppify/` gitignored. (`32e253e`)
- [x] **CI added** — `.github/workflows/ci.yml` (uv + pytest, Python 3.11–3.13) + README badge. Lint was not added then (pre-existing `src/llm` debt); since 2026-11-12 `./run.sh lint` is a step in the `gate` job.
- [x] **CITATION.cff + SECURITY.md added**.
- [x] **Doc indexes completed** — dialogs, discussions, speculative_design, languages, computer.
- ○ **Full paper regeneration** — still open; see **Open — 2026-11-12** item 1 (the Mermaid CLI is absent, so figures cannot be re-rendered).
- ○ **`.desloppify/` tracked artifacts** — still tracked as of 2026-11-12; see **Open — 2026-11-12** item 2.
- ○ **Zenodo deposit metadata** — the reason shifted: the latest record (`15231156`, behind concept DOI `10.5281/zenodo.15170907`) sits at version 1.4 and declares no license, while the README badge points at the v1.0 record `15170908`; see **Open — 2026-11-12** item 3.
- ○ **Static-analysis debt** — no longer unmeasured: the `debt` arm publishes flake8/black/isort/mypy counts in `docs/_generated/gate-metrics.md`; see **Open — 2026-11-12** item 4.

---

## Review + remediation pass — 2026-08-18

Fleet review pass. Baseline verified: **1254 passed, 0 failed** (full suite);
after this pass: **1273 passed, 0 failed** (19 net new tests).

### Completed

- [x] **FORMICA `update_belief` shadowed by a dead placeholder** — the
      placeholder 2-arg definition at `inference.py:271` overwrote the real
      3-arg Bayesian update, so the exported symbol (and everything imported
      from `beyond_cerebrum.src.operations`) raised `TypeError` on correct
      calls. Placeholder removed; real Bayesian update now exported.
- [x] **`beyond_cerebrum/tests/` stale, broken, and silently uncollected** —
      pytest.ini collects `beyond_cerebrum/src/tests`, so the outer tree
      (importing removed APIs: `SyntacticTree`, `AbstractStructure`,
      `SyntacticConstituent`, plus a `Generic[...]` subscript misuse) never ran
      and failed collection when invoked directly. Deleted; its unique
      coverage (inference, transformations) was re-authored against the
      current API in `beyond_cerebrum/src/tests/operations/` (30 new tests,
      all passing, zero mocks).
- [x] **`defaultdict` NameError** in `src/visualization/insect/animation_creator.py`
      `animate()` (used but never imported) — import added.
- [x] **Loop-variable capture in lambdas** (`src/examples/comprehensive_insect_simulation.py`)
      — three lambdas captured `insect`/`step` by reference; bound as default
      args.
- [x] **f-string brace escaping bug** in `src/lexicon/tests/test_end_to_end.py`
      — the generator template interpolated `{i+1}` from the outer scope
      (NameError at runtime) instead of writing a literal f-string into the
      generated parser; braces escaped.
- [x] **Dead shadowed imports** in `tests/core/test_neural_network.py`
      (`Visualizer`, `plot_case_linguistic_context` — both overridden by local
      definitions) removed.
- [x] **Stale generated `coverage.json` (641 KB) untracked** — `git rm --cached`;
      it is a point-in-time coverage report that stales on every run and is
      gitignore-class.

### Second pass — 2026-08-18 (live OpenRouter validation)

Suite after this pass: **1273 passed, 0 failed** (default run; live test
opt-in via `-m live` adds 1 more). Live key validated `moonshotai/kimi-k2`.

- [x] **Dead OpenRouter model slugs** — every configured default model 404s on
      the live API: `moonshotai/kimi-k2:free` ("unavailable for free — use
      moonshotai/kimi-k2"), `tngtech/deepseek-r1t2-chimera:free` ("No
      endpoints found"), and `anthropic/claude-3.5-sonnet` ("No endpoints
      found"). Replaced with live-verified slugs
      (`moonshotai/kimi-k2`, verified PONG) in `src/llm/config.py`,
      `src/lexicon/core/config.py`, `engine.py`, `run.py`,
      `batch_processor.py`, `examples/process_file.py`, `test_components.py`.
- [x] **Missing `_llm_entity_detection`** — the no-spaCy fallback path of
      `_detect_named_entities` called a method that did not exist
      (AttributeError on every run without spaCy). Implemented as a real LLM
      entity extractor with JSON parsing and field normalization.
- [x] **Lexicon component unification (the deferred Major)** —
      `process_text` now runs the unified component pipeline:
      `NLPPreprocessor` segments text (format detection, sentence split,
      NER/POS, coreference) and `CaseTagger` applies 8-case declension, with
      results surfaced as `result["segments"]` / `result["cased_segments"]`.
      Both stages degrade gracefully (warning log, never abort). Verified live
      end-to-end: "Alice met Bob in Paris..." → 2 segments, correct
      nominative tagging. Added `src/lexicon/tests/test_integration_live.py`
      (`live` marker, skipped without OPENROUTER_API_KEY).

### Third pass — 2026-08-18 (model chain + client fallback)

- [x] **OpenRouter client model-fallback chain** — `OpenRouterConfig` gained
      `fallback_models`; `chat_completion` now tries the requested model,
      then each fallback, when the model 404s (dead slug / no endpoints /
      account guardrail restriction — a permanent condition for that model,
      so a retry cannot help). Live-verified: dead default → both free
      models guardrail-blocked → qwen → success (PONG, stats show 4 requests,
      1 success).
- [x] **Model roster updated to the 2026-08 operator guidance** — primary
      `qwen/qwen3.8-27b` (live-verified), free-tier
      `nvidia/nemotron-3.5-lightning:free` and
      `dots-studio/dots-3-note-preview:free` as per-task primaries
      (both currently guardrail-blocked on this account; the fallback chain
      routes around them), `moonshotai/kimi-k2` as backstop. Applied across
      `src/llm/config.py`, `LexiconConfig` (incl. new
      `client_fallback_models`), engine wiring, CLI defaults, and examples.
      Stale static `get_available_models()` list refreshed.
- [x] Live engine E2E re-verified with the new chain (case tagging: Paris →
      LOCATIVE, Oxford → ABLATIVE). Live integration test loosened to assert
      the case-tagging mechanism (any populated slot) rather than a specific
      slot, which is model-dependent.

### Deferred (now tracked under "Open — 2026-11-12")

- ~~Lexicon component-architecture full unification~~ — **completed** in the
  second 2026-08-18 pass above.
- Full paper regeneration and the static-analysis debt are still open; their
  current blockers and owners are named under **Open — 2026-11-12** below.

---

## Open — 2026-11-12

The current open list, checked against the working tree, `git log` and the
`.gate/` artifacts on 2026-11-12. Nothing is listed here without a path, commit
or measured receipt behind it.

### 1. Full manuscript regeneration — blocked on the Mermaid CLI
- **Evidence**: `paper/output/` still holds the April 2025 builds
  (`CEREBRUM_1-4_April-16-2025.pdf`, `Figure_1.png`, …) while the package is
  `1.5.0` (`pyproject.toml`). The last commit touching `paper/` is `6837324`
  (#21), which only made failures fatal; it rebuilt nothing.
  `paper/assemble_paper.py` now exits nonzero when figure rendering, assembly,
  HTML or PDF conversion fails, so a partial rebuild cannot pass silently.
- **Blocker**: `mmdc` (Mermaid CLI) is missing on the review host
  (`command -v mmdc` → not found); `pandoc` and `xelatex` are installed. The
  figures under `paper/components/figures/` cannot be re-rendered without it.
- **Owner / unblock**: Daniel Ari Friedman (paper toolchain) — install
  `@mermaid-js/mermaid-cli`, then run `python paper/assemble_paper.py` and
  commit the regenerated artifacts.

### 2. `.desloppify/` review-session artifacts are still tracked
- **Evidence**: `git ls-files .desloppify` lists the 2026-03 external review
  sessions, e.g.
  `.desloppify/external_review_sessions/ext_20260306_020130_fb621885/review_result.json`
  and `.../claude_launch_prompt.md`. `.desloppify/` is already in `.gitignore`,
  so the tracked copies are the unfinished half of the 2026-08-02 note's
  `git rm -r .desloppify` recommendation.
- **Blocker**: none technical; it needs a maintainer call on whether those
  review transcripts belong in the public repository.
- **Owner / unblock**: Daniel Ari Friedman — decide, then
  `git rm -r --cached .desloppify`.

### 3. Zenodo deposit lags the repository, and the README badge points at v1.0
- **Evidence** (Zenodo API, 2026-11-12): record `15231156` — the latest version
  behind concept DOI `10.5281/zenodo.15170907` — reports `version: 1.4`,
  `publication_date: 2025-04-16` and no license field. Record `15170908`, the
  DOI in the README badge at `README.md:3`, reports `version: 1.0`. The
  repository is `1.5.0` under CC BY 4.0 (`pyproject.toml`, `LICENSE`,
  `CITATION.cff`).
- **Blocker**: publishing a new Zenodo version needs the depositor's account;
  repointing the badge and reconciling the citations is in-repo work that can
  happen now.
- **Owner / unblock**: Daniel Ari Friedman (Zenodo record owner) — deposit
  1.5.0 with the CC BY 4.0 license set, then make `README.md:3`,
  `README.md:41`, the README BibTeX entry and `CITATION.cff` name the same DOI.

### 4. Static-analysis debt is measured but not gated
- **Evidence**: `.gate/debt.json`, published in
  `docs/_generated/gate-metrics.md` (recorded 2026-11-12): flake8 over `src`
  9508 findings, `black --check src tests beyond_cerebrum/src` 247 files,
  `isort --check-only` on the same paths 187 files, `mypy src` 640 errors.
  `./run.sh lint` blocks only E9,F63,F7,F82.
- **Blocker**: the size of the backlog; no single change closes it.
- **Owner / unblock**: maintainers — ratchet one subtree per pull request and
  let the published counts show the movement.

### 5. Pre-commit hooks pin versions below the dev floors, and CI never runs pre-commit
- **Evidence**: `.pre-commit-config.yaml` pins `black` 23.7.0, `flake8` 6.0.0,
  `isort` 5.12.0, `mypy` v1.5.1 and `pre-commit-hooks` v4.4.0, while the
  `pyproject.toml` `dev` extra requires `black>=26.10.1`, `flake8>=7.4.1`,
  `isort>=9.0.2` and `mypy>=2.4.1`. No arm in `scripts/gate.py` runs
  pre-commit, and `.github/workflows/ci.yml` runs only `setup`, `lint`, `types`,
  `docs`, `test` and `gnn`.
- **Blocker**: none technical, but the black hook (`files: ^src/.*\.py$`) would
  fail on the current tree — `black --check` finds unformatted files under
  `src`, part of the 247 counted in item 4 — so the hook bump and the
  formatting have to land together.
- **Owner / unblock**: maintainers — bump the hook revisions, then either format
  `src` in the same change or scope the black hook while the debt shrinks.

---

## Backlog convention

- **Status** / **Owner** / **Last reviewed** headers at top are the authoritative
  record owner/currency.
- New review passes: delete completed items, re-file open items under the correct
  severity bucket, and refresh `Last reviewed`.
