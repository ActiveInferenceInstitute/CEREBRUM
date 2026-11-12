# CEREBRUM Scripts

Thin orchestration scripts for common CEREBRUM operations, plus the scripts the
gate runs. The gate itself is documented in
[../docs/RUN_GUIDE.md](../docs/RUN_GUIDE.md); enter through `../run.sh` rather
than calling these directly.

## Gate and documentation scripts

| Script | Purpose | Used by |
|--------|---------|---------|
| `gate.py` | defines every gate arm and times them into `.gate/timings.jsonl` | `../run.sh` |
| `gnn_checkout.py` | provisions the pinned GNN revision (`.gate/gnn`) with its own interpreter; the arm then runs `python -m gnn.importers` from it | `./run.sh gnn` |
| `check_links.py` | resolves every relative Markdown link; no network access | `./run.sh docs` |
| `docgen/doc_index.py` | generates `docs/documentation-index.md` from the docs tree | `./run.sh docs`, `./run.sh docs-index` |
| `docgen/gate_metrics.py` | renders `docs/_generated/gate-metrics.md` from `.gate/` artifacts | `./run.sh metrics` |
| `docgen/debt.py` | measures non-blocking static-analysis debt into `.gate/debt.json` | `./run.sh debt` |

## Paper and setup scripts

| Script | Purpose | Usage |
|--------|---------|-------|
| `setup_cerebrum.py` | Environment setup | `python scripts/setup_cerebrum.py` |
| `fix_markdown.py` | Fix markdown files | `python scripts/fix_markdown.py --cerebrum-dir CEREBRUM` |
| `render_diagrams.py` | Render Mermaid diagrams | `python scripts/render_diagrams.py` |
| `render_pdf.py` | Generate PDF from markdown | `python scripts/render_pdf.py --output output.pdf` |

## Design Philosophy

These scripts follow the **thin orchestrator** pattern:

- Scripts are simple wrappers over library code in `src/`
- All core logic lives in `src/utils/markdown/` and other modules
- Scripts handle CLI parsing and logging configuration
- Easy to extend with new orchestration flows
- The gate scripts are standard-library only, so `./run.sh setup`, `docs` and
  `metrics` still work before the environment exists

## Dependencies

The paper and setup scripts require the CEREBRUM package to be installed:

```bash
./run.sh setup    # uv sync --locked --extra dev --extra llm
```

The gate and documentation scripts need only Python 3.13's standard library
(plus the dev extras for the arms that run pytest, flake8 or mypy).
