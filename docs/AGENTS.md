# Documentation Context

## Purpose

This directory contains all documentation for the CEREBRUM framework, split into conceptual guides, technical specifications, and language-specific details.

## Structure

### Core Documentation

- **`cerebrum_core_spec.md`**: Source of truth for the core architecture
- **`getting_started.md`**: Quick start guide (uses `uv` for setup)
- **`how_it_works.md`**: Conceptual overview
- **`active_inference_integration.md`**: Active Inference principles

### Contributing Guides

- `contributing_research.md`, `contributing_technical.md`, `contributing_documentation.md`

### Subdirectories

Current contents per directory are listed in the generated
[`documentation-index.md`](documentation-index.md); do not restate the counts here.

| Directory | Contents |
| --------- | -------- |
| `diagrams/` | Mermaid diagram sources, templates, and the styling guide |
| `dialogs/` | Narrative dialog examples |
| `discussions/` | Community discussion archives |
| `examples/` | Detailed example playbooks |
| `insects/` | Insect cognition documents |
| `languages/` | Natural and constructed language analyses, plus `languages/computer/` |
| `speculative_design/` | Experimental design documents |

## Usage for Agents

- **Architecture questions**: Refer to `cerebrum_core_spec.md`
- **Creating diagrams**: Check `diagrams/style-guide.md`
- **Setup instructions**: Use `uv` as documented in `getting_started.md`, then run
  the gate (`../run.sh`); see [`RUN_GUIDE.md`](RUN_GUIDE.md)
- **Adding or moving a document**: run `../run.sh docs-index` and then
  `../run.sh docs`, which fails on a stale index or a broken relative link
