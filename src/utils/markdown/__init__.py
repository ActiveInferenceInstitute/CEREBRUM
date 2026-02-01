"""
CEREBRUM Markdown Utilities Module

This module provides utilities for processing Markdown files:
- utils: Core markdown manipulation (headings, images, figures)
- mermaid_renderer: Render Mermaid diagrams to PNG
- diagram_enhancer: Enhance diagram quality
- graphical_abstract: Generate graphical abstracts
"""

from .utils import (
    fix_image_references,
    standardize_heading_levels,
    extract_figure_info,
    create_figures_supplement,
    remove_images_from_main_text,
    ensure_figures_directory,
    get_project_root,
    cleanup_intermediate_files,
)

__all__ = [
    'fix_image_references',
    'standardize_heading_levels',
    'extract_figure_info',
    'create_figures_supplement',
    'remove_images_from_main_text',
    'ensure_figures_directory',
    'get_project_root',
    'cleanup_intermediate_files',
]
