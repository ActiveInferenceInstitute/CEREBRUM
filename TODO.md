# CEREBRUM — Project Backlog

> **Status**: Active
> **Owner**: Daniel Ari Friedman
> **Last reviewed**: 2026-08-18

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
- [x] **CI added** — `.github/workflows/ci.yml` (uv + pytest, Python 3.11–3.13) + README badge. Lint not added (pre-existing `src/llm` ruff debt).
- [x] **CITATION.cff + SECURITY.md added**.
- [x] **Doc indexes completed** — dialogs, discussions, speculative_design, languages, computer.
- ○ **Full paper regeneration** — deferred (heavy pandoc/xelatex/mmdc pipeline; committed artifacts self-consistent).
- ○ **`.desloppify/` tracked artifacts** — gitignored; recommend `git rm -r .desloppify` if it shouldn't be public.
- ○ **Zenodo deposit metadata** still NC-ND vs repo CC BY 4.0 — update the Zenodo record externally.
- ○ **`src/llm` ruff debt (168 pre-existing errors)** — out of scope for this docs pass.

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

### Deferred (unchanged)

- ~~Lexicon component-architecture full unification~~ — **completed** in the
  second 2026-08-18 pass above.
- Full paper regeneration (heavy pandoc/xelatex pipeline) — committed
  artifacts remain self-consistent.
- `src/llm` ruff debt — pre-existing, out of scope.

---

## Backlog convention

- **Status** / **Owner** / **Last reviewed** headers at top are the authoritative
  record owner/currency.
- New review passes: delete completed items, re-file open items under the correct
  severity bucket, and refresh `Last reviewed`.
