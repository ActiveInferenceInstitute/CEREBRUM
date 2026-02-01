# CEREBRUM Scripts

Thin orchestration scripts for common CEREBRUM operations.

## Available Scripts

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

## Dependencies

Scripts require the CEREBRUM package to be installed:

```bash
uv pip install -e ".[all]"
```
