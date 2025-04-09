# CEREBRUM 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15170908.svg)](https://doi.org/10.5281/zenodo.15170908)

CEREBRUM: Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling

## Repository
**GitHub Repository:** [https://github.com/ActiveInferenceInstitute/CEREBRUM](https://github.com/ActiveInferenceInstitute/CEREBRUM)

[![DOI](https://zenodo.org/badge/962267666.svg)](https://doi.org/10.5281/zenodo.15171283)

## Publication Information
- **Author:** Daniel Ari Friedman
- **Version:** 1.0
- **Date:** April 7, 2025
- **DOI:** [10.5281/zenodo.15170908](https://doi.org/10.5281/zenodo.15170908)
- **Zenodo Record:** [https://zenodo.org/records/15170908](https://zenodo.org/records/15170908)
- **Institution:** Active Inference Institute

## Overview
CEREBRUM is a unified modeling framework that integrates case-based reasoning with Bayesian representations.

## PDF Documentation
The full documentation for CEREBRUM is available as a PDF file (`CEREBRUM/CEREBRUM.pdf`), which is generated from the Markdown source files.

### Requirements for PDF Generation
To regenerate the PDF documentation, you need:
- Python 3
- pandoc
- A LaTeX distribution with xelatex (like TeX Live or MiKTeX)

### Generating the PDF
To regenerate the PDF documentation, run:

```bash
python3 tools/render_markdown.py
```

This script will:
1. Execute the `render_mermaids.py` script to generate diagrams from Mermaid syntax (unless skipped via arguments).
2. Process the main Markdown file (`CEREBRUM/CEREBRUM.md`).
3. Find, order, and include appendix files (matching `CEREBRUM/Supplement_*.md` by default).
4. Generate the final PDF file at `CEREBRUM/CEREBRUM.pdf`.

The PDF includes proper formatting, a table of contents, section numbering, and embedded figures.

## Citation
If you use CEREBRUM in your research, please cite:
```bibtex
@misc{friedman2025cerebrum,
  author = {Friedman, Daniel Ari},
  title = {CEREBRUM: Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling},
  year = {2025},
  month = {April},
  version = {1.0},
  doi = {10.5281/zenodo.15170908},
  publisher = {Active Inference Institute},
  url = {https://github.com/ActiveInferenceInstitute/CEREBRUM}
}
```

## License
CC BY-NC-ND 4.0

## Acknowledgements
This work builds upon the efforts and insights of many participants within the Active Inference Institute. Special thanks are extended to Dave Douglass for his contributions related to computational linguistics, archiving, Active Inference principles, upper ontologies, and translations, which have significantly informed aspects of this project.

## Contributing
We welcome contributions from researchers, developers, and enthusiasts across multiple domains:

### Ways to Contribute
- **🔬 Research**: Theoretical development, empirical testing, literature reviews
- **💻 Technical**: Core development, language implementations, integrations, testing
- **📚 Documentation**: Educational content, API docs, use cases, tutorials
- **🧩 Examples**: Model examples, domain applications, interactive demonstrations
- **🌍 Community**: Organizing events, content creation, mentoring, user support

### Contribution Scale
From micro-contributions (1-3 hours) to large projects (40+ hours), we value contributions of all sizes.

For detailed contribution guidelines, process information, and more specific ideas, please see our [CONTRIBUTING.md](CONTRIBUTING.md) document. 