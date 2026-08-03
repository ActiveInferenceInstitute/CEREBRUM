# CEREBRUM Documentation Review Log — 2026-08-02

Mega-deep documentation review pass (fleet task: one repo per agent). Scope:
every markdown file, README, AGENTS.md, code-adjacent docs, paper pipeline,
license/citation metadata, and missing high-value docs.

**Branch:** `main` · **Start HEAD:** `20ede70` · **End HEAD:** see git log.
**Test baseline (measured):** `pytest` → **1242 collected, 1239 passed, 3 skipped,
13 warnings** (numpy RuntimeWarnings in visualization tests), 198s.
Heavy suites (full paper regeneration via pandoc/xelatex/mmdc) were **not** run
— see Deferred.

---

## Findings by severity

### Minor (typos, broken links, formatting) — 9

1. README BibTeX had a duplicate `url` key (invalid BibTeX). → fixed
2. README showed two overlapping Zenodo badges (DOI badge + repo badge). → deduped
3. `.mermaid` files embedded as images in README (`![](...mermaid)`) — GitHub does not render them. → replaced with links
4. `docs/examples/Examples_README.md` linked to nonexistent `../CEREBRUM.md`. → fixed
5. `docs/insects/README.md` quick-start used `Case` without importing it. → fixed
6. `src/cases/README.md` listed nonexistent `examples.py`. → removed
7. `src/visualization/README.md` said "9 modules" for `insect/` (actual: 8). → fixed
8. `docs/diagrams/README.md` claimed `generated/` holds PNG/SVG (actual: `.mermaid` copies). → fixed
9. `docs/languages/computer/README.md` indexed 29 of 37 languages; `docs/languages/README.md` omitted Lean/Nim/OCaml. → completed

### Medium (stale sections, restructure, missing guides) — 12

1. **Stale test counts** in 5 files: `tests/README.md` (405), `tests/AGENTS.md`
   (1152/1149), root `AGENTS.md` (1152), `src/README.md` (253), `src/AGENTS.md`
   (1152, "0 warnings"). All aligned to the measured 1242/1239/3 (+13 warnings).
2. **License mismatch**: LICENSE file and `.aii` sidecar say CC BY 4.0; README,
   `docs/README.md`, `pyproject.toml` said CC BY-NC-ND 4.0. → aligned to CC BY 4.0.
   Note: the Zenodo *deposit metadata* still says NC-ND (external; update the
   record separately). The paper title page was left as-is to match the deposit.
3. **Three overlapping testing docs** (`tests/README.md`, `README_TESTING.md`,
   `Testing_README.md`) with contradictory commands (`pip`, `python3`, counts).
   → consolidated; `Testing_README.md` removed; commands unified to `uv run python -m pytest`.
4. **`docs/getting_started.md` described a fictional API** (`GenerativeModel`,
   `Case('nominative','NOM')`, `registry.register_model(model,'NOM')`,
   `implementations/js`, `implementations/rust` — none exist; `import cerebrum` fails).
   → fully rewritten against the verified real API; example executed and output captured.
5. **`docs/how_it_works.md` overclaimed implementation** of Precision Allocator /
   Message Bus (spec-only) and called JS/Rust "reference implementations"
   (only guidelines exist). → corrected.
6. **`docs/model_examples.md`** used `from cerebrum import ...` / `GenerativeModel`
   / `ParameterAccessPattern` / `Workflow` (nonexistent). → honest illustrative-style
   banner + imports fixed to the real API surface.
7. **`docs/examples/Examples_README.md` indexed only 5 of 8 examples.** → added 06–08.
8. **`docs/speculative_design/README.md`** had no file index (59 docs, unlisted) and
   contained a 0-byte `case_evolution_today.md`. → full 58-file index added; empty
   file deleted; 4 broken image refs (no such images exist) replaced with honest
   not-yet-rendered diagram notes.
9. **Insects status contradiction**: `assessment-summary.md` (no implementation) and
   `implementation-roadmap.md` (`# Implementation needed` + `pass`) vs
   `implementation-completion-summary.md` (complete — matches repo). → status
   banners added reconciling all three.
10. **`src/llm` documentation drift (MAJOR cluster, fixed as Medium/Large)**: README
    quick-start used nonexistent `LLMClient`; AGENTS.md exports listed 7 nonexistent
    classes; `docs/README.md` Module Structure listed 7 nonexistent modules and the
    API Reference linked 5 nonexistent pages; `__init__.py` docstring was vendored
    from another project ("Corym Library", `corym.llm` imports, fictional components).
    → all fixed to match the implemented package (`LLMUtils`, `OpenRouterAnalysisEngine`,
    `OpenRouterClient/Config/Conversation`, `ollama`); planned components explicitly
    marked not-yet-implemented.
11. **Contributing guides** (`contributing_technical/documentation/research.md`):
    links to 8 nonexistent docs, `examples/python|js|rust` dirs that don't exist,
    unverifiable Discord invites and email addresses. → replaced with real targets
    and GitHub issues/discussions.
12. **`beyond_cerebrum/README.md`** called `src/` a "placeholder" (it has real
    modules incl. `visualization/`); `beyond_cerebrum/docs/README.md` linked 5
    nonexistent module READMEs. → rewritten to actual state.

### Major (large overhauls, new systems) — 5

1. **Paper figure path convention drift**: `paper/README.md` + `assemble_paper.py`
   define bare `Figure_N.png` tags (relative to `paper/output/`), but all 17
   placement tags in 5 `main_text` components used `../figures/Figure_N.png`
   (unmatched by the script's regex → captions/renumbering silently skipped), and
   `assembled_paper.md` + `.html` carried broken `../figures/` refs. → normalized
   to the documented convention in components, assembled md, and html.
2. **Private artifact committed to a public repo**: `~/.claude/Plans/zazzy-brewing-wozniak.md`
   was tracked at repo root. → removed; `~/` and `.desloppify/` added to `.gitignore`.
3. **No CI**: repository had no `.github/` at all. → added a GitHub Actions workflow
   (uv + pytest on 3.11/3.12/3.13) + README badge. Lint deliberately NOT added to CI
   (src/llm carries 168 pre-existing ruff errors).
4. **No CITATION.cff / SECURITY.md**: → added both (grounded in the real DOI,
   author ORCID, version 1.5.0, license CC BY 4.0).
5. **Doc indexes**: `docs/languages/README.md` had 90+ non-clickable backtick
   filenames (fixed to links, incl. correct `computer/` prefix); stale gap/roadmap
   lists corrected; `docs/dialogs/` and `docs/discussions/` READMEs had no file
   indexes (added). Root `AGENTS.md` referenced a nonexistent `PROGRESS_REPORT.md`.

---

## Implemented (commits, in order)

1. `32e253e` chore: remove accidentally committed personal `~/.claude` artifact; ignore personal tool dirs
2. `0e42932` docs: align license metadata (CC BY 4.0), test counts (1242/1239/3), consolidate tests READMEs
3. `f19d991` docs: rewrite getting_started against the real API; correct architecture overclaims
4. `f1f08f9` docs: complete example/speculative/insects/language index fixes
5. `8e2761f` docs: fix src/llm documentation drift (fictional API -> implemented reality)
6. `7b1fb51` docs: fix contributing guides, beyond_cerebrum docs, and module README inaccuracies
7. `60de1e1` docs: align paper figure placement tags with the documented build convention
8. (final) chore/docs: CI workflow, CITATION.cff, SECURITY.md, README badges, REVIEW_LOG, TODO

## Deferred / open items (see TODO.md)

- **Full paper regeneration** (pandoc + xelatex + mmdc) not run: heavy toolchain,
  large binary churn; committed PDF/HTML are self-consistent. Run `python3 paper/assemble_paper.py`
  when a paper change lands.
- **`.desloppify/`** (68 tracked files of internal review-tool sessions) still tracked;
  gitignored now. Recommend `git rm -r .desloppify` if the org decides it shouldn't be public.
- **`src/llm` ruff debt**: 168 pre-existing errors (import sorting, typing.List
  deprecation, etc.) — unrelated to this pass; `src/llm/__init__.py` `__version__ = "1.0.0"`
  is the submodule's own version (kept).
- **Zenodo deposit metadata** (paper record) still CC BY-NC-ND while repo LICENSE is
  CC BY 4.0 — update the Zenodo record to match.
- **`assembled_paper.md` version metadata** ("1.2 (2025-04-12)") vs repo version
  1.5.0 — appears to be intentional paper-version metadata; confirm before changing.
- **`docs/speculative_design/beast_case_analysis.md:283`** explicit research
  placeholders — intentional design notes, kept.
- **LLM planned components** (`PerplexityResearcher` et al.) — documented as planned;
  no code exists yet.
