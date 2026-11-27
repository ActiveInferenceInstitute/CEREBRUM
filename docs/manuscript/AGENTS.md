# Paper Generation Context

## Purpose

This directory contains the tools and source files for generating the academic paper for CEREBRUM.

## Workflow

1. **Source**: Content is located in `components/`.
2. **Generation**: The `assemble_paper.py` script aggregates components and figures.
3. **Output**: The [template render](README.md) writes the validated PDF into repository-root `output/`. The legacy assembler writes to `paper/output/`.

## Key Scripts

- `assemble_paper.py`: The orchestration script. It handles Mermaid diagram rendering (via Node.js) and Pandoc conversion.
