# CEREBRUM — Project Backlog

> **Status**: Active
> **Owner**: Daniel Ari Friedman
> **Last reviewed**: 2026-08-01

This is the authoritative project backlog. It records completed work and the
remaining **Major**-class items from the hostile red-team review pass of
2026-08-01. The vast majority of review findings (Minor, Medium, and the bulk
of the Major) were implemented during the 2026-08-01 review + remediation passes;
the items below under "Major — Scoped (deferred)" are the few that remain open.

---

## Completed / Closed

Implemented during the 2026-08-01 review/remediation passes. Full `pytest tests/`
suite passes: **1178 passed, 3 skipped, 0 failed** (verified EXIT=0 on a clean
run; the baseline was 1 failed / 1144 passed / 3 skipped). Targeted module
suites (core, cases, transformations, utils/data_generator, lexicon, visualization)
all green (442 passed in the combined post-isort check).

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

---

## Major — Scoped (deferred)

These are the **remaining** Major-class items intentionally NOT implemented this
pass. Each is concrete and actionable.

### 1. Lexicon component-architecture full unification (partial — data integrity done)
- **Files**: `src/lexicon/core/engine.py` getters `_get_nlp_preprocessor` /
  `_get_case_tagger` / `_get_paraphrase_generator`; the
  `NLPPreprocessor`/`CaseTagger` classes
- **Why it matters**: The **data-integrity portion is done and verified** —
  `_get_entity_deduplicator` + `_get_graph_assembler` are wired into
  `_build_knowledge_graph`, producing deduplicated, stable CID-based node/claim
  IDs (verified: 3 input entities incl. a duplicate → 2 unique nodes, stable IDs).
  What remains is the larger architectural unification: the shipped `process_text`
  path still uses its own inline LLM logic while the spaCy-based `NLPPreprocessor` /
  `CaseTagger` component classes exist as a separate (largely unused) pipeline.
- **Suggested fix**: Continue wiring `NLPPreprocessor`/`CaseTagger` into
  `process_text` and remove the parallel inline implementation, or delete the
  unused classes; add an integration test on the real path. This is intentionally
  deferred because it is a large refactor of a live-API-dependent path that cannot
  be fully validated without a real OpenRouter key/network.

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

## Backlog convention

- **Status** / **Owner** / **Last reviewed** headers at top are the authoritative
  record owner/currency.
- New review passes: delete completed items, re-file open items under the correct
  severity bucket, and refresh `Last reviewed`.
