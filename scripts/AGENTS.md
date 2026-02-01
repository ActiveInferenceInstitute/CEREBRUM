# Scripts Context

## Purpose

Thin orchestration scripts that wrap library code from `src/`.

## Structure

```text
scripts/
├── setup_cerebrum.py    # Environment setup
├── fix_markdown.py      # Markdown fixing wrapper
├── render_diagrams.py   # Mermaid rendering wrapper  
├── render_pdf.py        # PDF generation wrapper
└── README.md
```

## Pattern

All scripts import from `src/` and provide CLI interfaces.
Core business logic lives in `src/utils/markdown/` and other modules.

## Testing

Scripts delegate to tested library code.
Integration tests for scripts belong in `src/tests/`.
