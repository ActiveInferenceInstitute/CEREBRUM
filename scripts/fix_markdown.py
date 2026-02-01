#!/usr/bin/env python3
"""
Fix Markdown Files - Thin Orchestration Script

This script wraps the markdown utilities in src/utils/markdown to fix
markdown files in the CEREBRUM project directory.

Usage:
    python scripts/fix_markdown.py [--cerebrum-dir CEREBRUM] [--verbose]
"""

import sys
import logging
import argparse
from pathlib import Path

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.utils.markdown import (
    fix_image_references,
    standardize_heading_levels,
    ensure_figures_directory,
    extract_figure_info,
    create_figures_supplement,
    remove_images_from_main_text,
    get_project_root,
)

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)


def fix_main_document(cerebrum_dir: Path, main_file: Path) -> bool:
    """Fix the main CEREBRUM document."""
    logger.info(f"Fixing main document: {main_file}")
    
    if not main_file.exists():
        logger.error(f"Main file not found: {main_file}")
        return False
    
    ensure_figures_directory(cerebrum_dir)
    fix_image_references(main_file, output_to_figures=True)
    figure_info = extract_figure_info(main_file)
    create_figures_supplement(cerebrum_dir, figure_info)
    remove_images_from_main_text(main_file)
    standardize_heading_levels(main_file, is_main_text=True)
    
    return True


def fix_supplement_files(cerebrum_dir: Path) -> list:
    """Find and fix all supplement files."""
    logger.info("Fixing supplement files")
    
    processed = []
    for supp_file in sorted(cerebrum_dir.glob("Supplement_*.md")):
        if supp_file.name != "Supplement_0_Figures.md":
            fix_image_references(supp_file)
            standardize_heading_levels(supp_file)
            processed.append(supp_file)
            logger.info(f"Fixed: {supp_file.name}")
    
    return processed


def main():
    parser = argparse.ArgumentParser(description="Fix CEREBRUM Markdown Files")
    parser.add_argument("--cerebrum-dir", default="CEREBRUM",
                        help="Directory containing CEREBRUM files")
    parser.add_argument("--main-file", default="CEREBRUM_main_text.md",
                        help="Main Markdown file")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Enable verbose logging")
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    project_root = get_project_root()
    cerebrum_dir = project_root / args.cerebrum_dir
    main_file = cerebrum_dir / args.main_file
    
    if not cerebrum_dir.exists():
        logger.error(f"CEREBRUM directory not found: {cerebrum_dir}")
        return 1
    
    # Fix title page
    title_page = cerebrum_dir / "title_page.md"
    if title_page.exists():
        standardize_heading_levels(title_page)
    
    # Fix main document
    if not fix_main_document(cerebrum_dir, main_file):
        return 1
    
    # Fix supplements
    fixed = fix_supplement_files(cerebrum_dir)
    logger.info(f"Fixed {len(fixed)} supplement files")
    
    logger.info("All Markdown files have been fixed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
