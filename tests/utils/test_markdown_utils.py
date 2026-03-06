"""
Tests for src/utils/markdown/utils.py and src/utils/markdown/diagram_enhancer.py

Tests fix_image_references, standardize_heading_levels, extract_figure_info,
remove_images_from_main_text, cleanup_intermediate_files, ensure_figures_directory.
Also tests DiagramEnhancer: _add_config_header, _enhance_styling, _add_case_annotations,
_generate_state_diagram, _generate_sequence_diagram, generate_new_diagrams.
"""

import pytest
from pathlib import Path

from src.utils.markdown.utils import (
    fix_image_references,
    standardize_heading_levels,
    extract_figure_info,
    remove_images_from_main_text,
    ensure_figures_directory,
    cleanup_intermediate_files,
    get_project_root,
)
from src.utils.markdown.diagram_enhancer import DiagramEnhancer


# ─── Markdown utils.py ───────────────────────────────────────────────

class TestFixImageReferences:
    def test_basic_fix(self, tmp_path):
        md = tmp_path / "test.md"
        md.write_text("![Figure 1: Test](output/Figure_1.png)\n")
        result = fix_image_references(md, output_to_figures=True)
        assert result is True
        content = md.read_text()
        assert "figures/Figure_1.png" in content

    def test_double_labeled_fix(self, tmp_path):
        md = tmp_path / "test.md"
        md.write_text("![Figure 1: Figure 1: Duplicate Label](output/Figure_1.png)\n")
        fix_image_references(md)
        content = md.read_text()
        assert "Figure 1: Figure 1:" not in content

    def test_remove_images(self, tmp_path):
        md = tmp_path / "test.md"
        md.write_text("# Title\n\n![Figure 1: Caption](figures/Figure_1.png)\n\nText\n")
        fix_image_references(md, remove_images=True)
        content = md.read_text()
        assert "![" not in content
        assert "Text" in content


class TestStandardizeHeadingLevels:
    def test_main_text_adds_title(self, tmp_path):
        md = tmp_path / "main.md"
        md.write_text("# Overview\n\nContent here\n")
        result = standardize_heading_levels(md, is_main_text=True)
        assert result is True
        content = md.read_text()
        assert "# Main text" in content

    def test_supplement_numbering(self, tmp_path):
        md = tmp_path / "Supplement_1_Methods.md"
        md.write_text("# Methods\n\n## Section A\n\n## Section B\n")
        result = standardize_heading_levels(md)
        assert result is True
        content = md.read_text()
        assert "Supplement 1:" in content
        assert "1.1" in content or "1.2" in content

    def test_non_supplement_keeps_heading(self, tmp_path):
        md = tmp_path / "title_page.md"
        md.write_text("# My Title\n\nContent\n")
        standardize_heading_levels(md)
        content = md.read_text()
        assert "# My Title" in content


class TestExtractFigureInfo:
    def test_extracts_figures(self, tmp_path):
        md = tmp_path / "main.md"
        md.write_text(
            "Figure 1 shows the architecture.\n"
            "![Figure 1: Foundation Domains](figures/Figure_1.png)\n"
            "Figure 2 shows the workflow.\n"
        )
        figures = extract_figure_info(md)
        assert len(figures) >= 1
        assert figures[0]["number"] == 1

    def test_returns_empty_for_missing_file(self, tmp_path):
        md = tmp_path / "nonexistent.md"
        figures = extract_figure_info(md)
        assert figures == []


class TestRemoveImagesFromMainText:
    def test_removes_images(self, tmp_path):
        md = tmp_path / "main.md"
        md.write_text("Text\n![Figure 1: Caption](figures/Figure_1.png)\nMore text\n")
        result = remove_images_from_main_text(md)
        assert result is True
        content = md.read_text()
        assert "![" not in content
        assert "More text" in content


class TestEnsureFiguresDirectory:
    def test_creates_directory(self, tmp_path):
        figures_dir = ensure_figures_directory(tmp_path)
        assert figures_dir.exists()
        assert figures_dir.is_dir()

    def test_copies_from_output(self, tmp_path):
        output_dir = tmp_path / "output"
        output_dir.mkdir()
        (output_dir / "Figure_1.png").write_bytes(b"fake_png_data")
        figures_dir = ensure_figures_directory(tmp_path)
        assert (figures_dir / "Figure_1.png").exists()


class TestCleanupIntermediateFiles:
    def test_removes_aux_files(self, tmp_path):
        (tmp_path / "test.aux").write_text("aux data")
        (tmp_path / "test.log").write_text("log data")
        (tmp_path / "test.md").write_text("keep this")
        cleanup_intermediate_files(tmp_path)
        assert not (tmp_path / "test.aux").exists()
        assert not (tmp_path / "test.log").exists()
        assert (tmp_path / "test.md").exists()


class TestGetProjectRoot:
    def test_returns_path(self):
        root = get_project_root()
        assert isinstance(root, Path)


# ─── DiagramEnhancer ─────────────────────────────────────────────────

class TestDiagramEnhancer:
    @pytest.fixture
    def enhancer(self, tmp_path):
        # Pass a non-existent config path so it uses defaults
        return DiagramEnhancer(config_path=str(tmp_path / "nonexistent.yaml"))

    def test_init_with_defaults(self, enhancer):
        assert enhancer.config == {}
        assert enhancer.cerebrum_colors["primary"] == "#4ECDC4"

    def test_add_config_header(self, enhancer):
        result = enhancer._add_config_header("graph TD\n  A --> B")
        assert result.startswith("---")
        assert "graph TD" in result

    def test_enhance_styling_adds_classdefs(self, enhancer):
        content = "graph TD\n  A --> B"
        result = enhancer._enhance_styling(content)
        assert "classDef framework" in result

    def test_enhance_styling_skips_if_present(self, enhancer):
        content = "classDef myclass fill:#fff\ngraph TD\n  A --> B"
        result = enhancer._enhance_styling(content)
        assert content == result  # Should not modify

    def test_add_case_annotations(self, enhancer):
        content = 'A["input source"] --> B["model engine"]'
        result = enhancer._add_case_annotations(content)
        assert isinstance(result, str)

    def test_generate_new_diagrams(self, enhancer, tmp_path):
        output_dir = tmp_path / "diagrams"
        enhancer.generate_new_diagrams(str(output_dir))
        # Should generate at least one file
        generated = list(output_dir.glob("*.mermaid"))
        assert len(generated) >= 1

    def test_generate_state_diagram(self, enhancer, tmp_path):
        enhancer._generate_state_diagram(tmp_path)
        assert (tmp_path / "model_lifecycle_states.mermaid").exists()

    def test_generate_sequence_diagram(self, enhancer, tmp_path):
        enhancer._generate_sequence_diagram(tmp_path)
        assert (tmp_path / "api_interaction_sequence.mermaid").exists()

    def test_extract_diagrams_from_template(self, enhancer):
        template = "```markdown\ngraph TD\nA --> B\n```\n\nText\n\n```markdown\nflowchart LR\nX --> Y\n```"
        diagrams = enhancer._extract_diagrams_from_template(template)
        assert len(diagrams) >= 1

    def test_enhance_existing_diagrams(self, enhancer, tmp_path):
        input_dir = tmp_path / "input"
        input_dir.mkdir()
        (input_dir / "test.mermaid").write_text("graph TD\n  A --> B\n")
        output_dir = tmp_path / "output"
        enhancer.enhance_existing_diagrams(str(input_dir), str(output_dir))
        assert (output_dir / "test.mermaid").exists()
        content = (output_dir / "test.mermaid").read_text()
        assert "---" in content  # config header added

    def test_get_interactive_elements_architecture(self, enhancer):
        result = enhancer._get_interactive_elements("architecture_overview.mermaid")
        assert "CEREBRUM" in result

    def test_apply_templates_missing(self, enhancer, tmp_path):
        # Should not crash on missing template
        enhancer.apply_templates("nonexistent", str(tmp_path / "out"))
