# Diagrams Context

## Purpose

Visual documentation for CEREBRUM architecture and workflows.

## Contents

```text
docs/diagrams/
├── AGENTS.md                        # This file
├── README.md                        # Diagram index
├── style-guide.md                   # Visual conventions
├── improvement_plan.md              # Planned diagram improvements
├── config.yaml                      # Mermaid rendering config
├── templates/                       # Reusable diagram templates
├── generated/                       # Rendered PNG/SVG outputs
├── api_sequence.mermaid             # API interaction sequence
├── data_flow.mermaid                # Data flow diagram
├── implementation_workflow.mermaid  # Dev workflow
├── model_lifecycle.mermaid          # Model lifecycle stages
├── project_architecture.mermaid     # Overall architecture
└── use_cases_mindmap.mermaid        # Use cases overview
```

## Workflow

1. Edit `.mermaid` source files
2. Render with `scripts/render_diagrams.py` or Mermaid CLI
3. Generated images go to `generated/`

## Style

Follow conventions in `style-guide.md` for consistent visual language.
