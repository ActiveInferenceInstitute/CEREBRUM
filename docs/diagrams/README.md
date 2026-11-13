# Diagrams

This directory contains Mermaid diagram source files and related assets for CEREBRUM documentation.

## Contents

- **Mermaid Sources**: `.mermaid` files for project architecture, data flow, workflows, etc.
- **`generated/`**: Rendered diagram copies (`.mermaid`) produced by `scripts/render_diagrams.py`
- **`templates/`**: Reusable diagram templates.
- **`style-guide.md`**: Diagram styling conventions.
- **[`improvement_plan.md`](improvement_plan.md)**: Roadmap for diagram enhancements.

## Usage

Diagrams are rendered with `@mermaid-js/mermaid-cli`, pinned to `12.x` in the
repository-root `package.json` (`npm install` there first). The producers call
`mmdc` like this (`src/utils/markdown/mermaid_renderer.py`):

```bash
mmdc -i diagram.mmd -o diagram.png -b transparent --size 2400 -c config.json -s 2.5
```

`paper/assemble_paper.py` renders paper figures with `--size 1200` instead.
mermaid-cli 12 removed `-w/--width` and `-H/--height`: `--size` bounds the
longest rendered side in CSS px, while `-s` is Puppeteer's scale factor, so
`--size 2400 -s 2.5` yields a PNG up to 6000 px on its longest side.

`config.yaml` holds Mermaid theme variables and styling conventions, not CLI
options; each renderer passes its own JSON config via `-c`.
