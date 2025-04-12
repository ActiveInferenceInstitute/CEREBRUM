# CEREBRUM Paper Structure & Formatting Guidelines

This document provides guidelines for maintaining consistency in the CEREBRUM paper's structure, formatting, and component integration.

## Directory Structure

```
paper/
├── assemble_paper.py          # Main assembly script
├── README.md                  # This file (guidelines)
├── components/                # All paper components
│   ├── main_text/             # Primary paper sections (numbered)
│   │   ├── 1_title_page.md
│   │   ├── 2_abstract.md
│   │   └── ...
│   ├── figures/               # Figure definitions (Mermaid)
│   │   ├── Figure_1.md
│   │   ├── Figure_2.md
│   │   └── ...
│   ├── supplemental_sections/ # Supplementary material
│   │   ├── Supplement_1_*.md
│   │   ├── Supplement_2_*.md
│   │   └── ...
│   └── templates/             # Templates for new components
│       ├── Figure_template.md
│       ├── Section_template.md
│       └── Supplement_template.md
└── output/                    # Generated files
    ├── assembled_paper.md     # Combined markdown
    ├── assembled_paper.html   # HTML version
    ├── assembled_paper.pdf    # PDF version
    ├── Figure_1.png           # Rendered figures
    └── ...
```

## Building the Paper

The paper is built using the `assemble_paper.py` script, which performs the following steps:

1. Renders Mermaid diagrams from figure definition files to PNG images
2. Assembles all markdown components in the correct order
3. Generates HTML and PDF versions of the complete paper

### Prerequisites

To build the paper, you need:

- Python 3
- Node.js with Mermaid CLI (`@mermaid-js/mermaid-cli` package)
- Pandoc
- XeLaTeX (for PDF generation)

### Building the Paper

To build the complete paper (images, markdown, HTML, and PDF):

```bash
python3 paper/assemble_paper.py
```

Run this command from the root directory of the CEREBRUM repository. The script will:

1. Generate PNG images for all Mermaid diagrams in `paper/components/figures/`
2. Assemble the complete paper in markdown format at `paper/output/assembled_paper.md`
3. Create an HTML version at `paper/output/assembled_paper.html`
4. Generate a PDF version at `paper/output/assembled_paper.pdf`

All output files will be placed in the `paper/output/` directory.

## Component Naming Conventions

### Main Text Files

Name main text files with numeric prefixes to enforce ordering:
- `1_title_page.md`
- `2_abstract.md`
- `3_introduction.md`
- etc.

### Figure Files

Name figure files consistently:
- `Figure_1.md` (containing Mermaid code)
- `Figure_2.md`
- etc.

### Supplemental Sections

Name supplemental sections with clear numeric prefixes:
- `Supplement_1_Mathematical_Formalization.md`
- `Supplement_2_Novel_Linguistic_Cases.md`
- etc.

## Figure Management

### Figure Definition (in `components/figures/`)

Each figure should be defined in its own Markdown file with:
1. A title that matches the figure number
2. A Mermaid diagram definition

Example (`Figure_1.md`):
```md
# Figure 1: Foundation Domains of CEREBRUM

```mermaid
flowchart TB    
    // Mermaid diagram code here
```
```

### Figure References (in main text)

Reference figures using pandoc-crossref syntax:

```md
@fig:fig1 illustrates the framework components.

![Caption text.](Figure_1.png){#fig:fig1}
```

Key elements:
- Use `@fig:figX` for in-text references
- Image syntax: `![Caption text.](Figure_X.png){#fig:figX}`
- End caption text with a period
- Use consistent labels: `#fig:fig1`, `#fig:fig2`, etc.
- Remove "Figure X:" from the caption text (pandoc-crossref adds it automatically)

### Figure Placement

Place figure image tags where you want the figure to appear in the document, after referencing it in the text.

## Sections and Headers

- Use ATX-style headers (`#`, `##`, `###`, etc.)
- Main section titles use `##` (level 2)
- Subsections use `###` (level 3)
- Avoid skipping levels
- Title page uses a single `#` (level 1) for the document title

## Tables

Format tables using standard Markdown table syntax:

```md
**Table 1: Example Table Title**
| Column 1 | Column 2 | Column 3 |
|----------|----------|----------|
| Data 1   | Data 2   | Data 3   |
| Data 4   | Data 5   | Data 6   |
```

## Equations and Math

Use LaTeX syntax for equations:

- Inline math: `$E = mc^2$`
- Display math: 
  ```
  $$
  F = D_{KL}[q(s|T(m))||p(s|m)] - \mathbb{E}_{p}[\log p(o|s,T(m))]
  $$
  ```
- For numbered equations, use the `\tag{}` command:
  ```
  $$
  F = D_{KL}[q(s|T(m))||p(s|m)] - \mathbb{E}_{p}[\log p(o|s,T(m))]  \tag{1}
  $$
  ```

## Cross-References

- Figures: `@fig:figX` (e.g., `@fig:fig1`)
- Equations: Can be referenced by their tag number: "As shown in equation (1)"
- Sections: No automatic numbering; reference by name

## Assembly Process

The paper is assembled using `assemble_paper.py`, which:

1. Renders Mermaid diagrams from figure files
2. Combines markdown components in order
3. Generates HTML and PDF outputs

To build the paper:

```bash
python3 paper/assemble_paper.py
```

## Pandoc Extensions

The paper uses several Pandoc extensions for formatting:

- **booktabs**: For better table formatting in PDF output
- **pandoc-crossref**: For automatic figure numbering and references (when installed)

## Content Guidelines

- Maintain consistent terminology throughout
- Use descriptive figure captions that stand alone
- Ensure all figures are referenced in the text before they appear
- Avoid placeholder text
- Include proper citations where applicable
- Maintain section balance and flow

## Available Cases

When referring to cases in the text, use the standard abbreviations:

- **[NOM]** - Nominative
- **[ACC]** - Accusative
- **[DAT]** - Dative
- **[GEN]** - Genitive
- **[INS]** - Instrumental
- **[LOC]** - Locative
- **[ABL]** - Ablative
- **[VOC]** - Vocative

## Using Templates for New Components

Templates are provided for creating new components with consistent formatting:

1. **Figure Template** (`components/templates/Figure_template.md`):
   - Copy this template when creating a new figure
   - Update the figure number and title
   - Replace the placeholder Mermaid code with your diagram

2. **Section Template** (`components/templates/Section_template.md`):
   - Use this template when adding a new main text section
   - Provides structure for sections, subsections, figures, tables, and equations
   - Save with appropriate numeric prefix in `components/main_text/`

3. **Supplement Template** (`components/templates/Supplement_template.md`):
   - Template for creating new supplemental sections
   - Includes standard sections and formatting for supplements
   - Save with appropriate naming in `components/supplemental_sections/`

## Schema Extension

When adding new components:

1. For new main text sections:
   - Copy the Section template from `paper/components/templates/`
   - Add to `paper/components/main_text/` with appropriate numeric prefix
   - Ensure headers use correct levels (starting with `##`)

2. For new figures:
   - Copy the Figure template from `paper/components/templates/`
   - Add figure definition to `paper/components/figures/`
   - Reference properly in main text

3. For new supplements:
   - Copy the Supplement template from `paper/components/templates/`
   - Add to `paper/components/supplemental_sections/`
   - Use consistent naming convention 