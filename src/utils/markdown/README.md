# Markdown Utilities

Utility functions for processing Markdown files in the CEREBRUM project.

## Contents

| File | Purpose |
|------|---------|
| `utils.py` | Core markdown manipulation (headings, images, figures) |
| `mermaid_renderer.py` | Render Mermaid diagrams to PNG |
| `diagram_enhancer.py` | Enhance diagram quality |
| `graphical_abstract.py` | Generate graphical abstracts |

## Usage

```python
from src.utils.markdown import (
    fix_image_references,
    standardize_heading_levels,
    get_project_root,
)

# Fix all image references in a markdown file
fix_image_references(Path("CEREBRUM/main.md"))

# Standardize heading levels
standardize_heading_levels(Path("document.md"), is_main_text=True)
```

## CLI Usage

These utilities are wrapped by thin orchestration scripts in `scripts/`:

```bash
python scripts/fix_markdown.py --cerebrum-dir CEREBRUM
python scripts/render_pdf.py --output output.pdf
python scripts/render_diagrams.py
```
