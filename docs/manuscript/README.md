# Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling (CEREBRUM)

This directory contains the existing CEREBRUM paper and all twelve
supplements. The [paper release](https://zenodo.org/records/15231156)
remains the historical publication. The legacy
[assembler](../../paper/README.md) reads the same source tree.

## Authoring

The [configuration](config.yaml) orders sections and supplements. The
[editable diagrams](components/figures/) and [figure registry](assets/README.md)
document the illustrations used in the PDF.

- [2 abstract](components/main_text/2_abstract.md)
- [3 introduction](components/main_text/3_introduction.md)
- [4 methods](components/main_text/4_methods.md)
- [5 case functions in cognitive models](components/main_text/5_case_functions_in_cognitive_models.md)
- [6 model workflows as case transformations](components/main_text/6_model_workflows_as_case_transformations.md)
- [7 active inference models](components/main_text/7_active_inference_models.md)
- [8 conclusion](components/main_text/8_conclusion.md)
- [Supplement 1 Mathematical Formalization](components/supplemental_sections/Supplement_1_Mathematical_Formalization.md)
- [Supplement 2 Novel Linguistic Cases](components/supplemental_sections/Supplement_2_Novel_Linguistic_Cases.md)
- [Supplement 3 Practical Applications](components/supplemental_sections/Supplement_3_Practical_Applications.md)
- [Supplement 4 Related Work](components/supplemental_sections/Supplement_4_Related_Work.md)
- [Supplement 5 Category Theory](components/supplemental_sections/Supplement_5_Category_Theory.md)
- [Supplement 6 Future Directions](components/supplemental_sections/Supplement_6_Future_Directions.md)
- [Supplement 7 Computational Complexity](components/supplemental_sections/Supplement_7_Computational_Complexity.md)
- [Supplement 8 Active Inference Formulation Details](components/supplemental_sections/Supplement_8_Active_Inference_Formulation_Details.md)
- [Supplement 9 Mathematical Foundations](components/supplemental_sections/Supplement_9_Mathematical_Foundations.md)
- [Supplement 10 Algorithmic Details Pseudocode](components/supplemental_sections/Supplement_10_Algorithmic_Details_Pseudocode.md)
- [Supplement 11 Formal Definitions Core Linguistic Cases](components/supplemental_sections/Supplement_11_Formal_Definitions_Core_Linguistic_Cases.md)
- [Supplement 12 Writing Research Paper](components/supplemental_sections/Supplement_12_Writing_Research_Paper.md)

## Render and validate

Use a clean checkout named `CEREBRUM` and the [pinned docxology renderer](https://github.com/docxology/template/tree/87704feae0a4490ef7dad9c931d268e8c30273c2).
Install Git, uv, Pandoc, XeLaTeX, and the DejaVu font family in addition to the packages listed in the
[template setup guide](https://github.com/docxology/template/blob/87704feae0a4490ef7dad9c931d268e8c30273c2/docs/guides/startup-and-setup.md).
Run from the root of the source checkout.

```sh
source_checkout="$(pwd -P)"
source_name="${source_checkout##*/}"
renderer_checkout="$(mktemp -d)/template"
mkdir -p output/figures
cp paper/output/Figure_*.png docs/manuscript/assets/figure_registry.json output/figures/
git init "$renderer_checkout"
git -C "$renderer_checkout" fetch --depth=1 https://github.com/docxology/template.git 87704feae0a4490ef7dad9c931d268e8c30273c2
git -C "$renderer_checkout" checkout --detach FETCH_HEAD

cd "$renderer_checkout"
uv sync --locked
mkdir -p projects/working
ln -s "$source_checkout" "projects/working/$source_name"
uv run --no-sync python scripts/pipeline/stage_03_render.py --project "working/$source_name"
uv run --no-sync python scripts/pipeline/stage_04_validate.py --project "working/$source_name"
```

Stage 3 writes the combined PDF to `output/pdf/CEREBRUM_combined.pdf` in
the source checkout. Stage 4 records its verdict in
`output/reports/validation_report.md`. The source and artifact bindings are
recorded in `output/reports/rendered_provenance.json`. Use a disposable
checkout to keep the generated files separate from source edits.
